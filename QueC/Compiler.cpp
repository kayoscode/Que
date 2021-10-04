#define _CRT_SECURE_NO_WARNINGS

#include "Compiler.h"
#include "../QueASM/Assembler.h"

uint32_t encodeInstruction(uint8_t opcode, bool immediateFlag, uint8_t op1, uint8_t op2, uint8_t op3) {
	uint32_t finalInstruction = opcode;
	finalInstruction |= immediateFlag << 7;
	finalInstruction |= op1 << 8;
	finalInstruction |= op2 << 13;
	finalInstruction |= op3 << 18;

	return finalInstruction;
}

bool ReserveTableCompare::operator()(const std::string& a, const std::string& b) const {
	return strcmp(a.c_str(), b.c_str()) < 0;
}

Compiler::Compiler() 
	:symbolStack(this),
	types()
{
	// Create reserve words (200 series)
	int currentCode = 200;
	reserveTable.addReserveWord("var", currentCode++);
	reserveTable.addReserveWord("constant", currentCode++);
	reserveTable.addReserveWord("export", currentCode++);
	reserveTable.addReserveWord("import", currentCode++);
	reserveTable.addReserveWord("true", currentCode++);
	reserveTable.addReserveWord("false", currentCode++);
	reserveTable.addReserveWord("ref", currentCode++);
	reserveTable.addReserveWord("deref", currentCode++);
	reserveTable.addReserveWord("return", currentCode++);
	reserveTable.addReserveWord("sizeof", currentCode++);
	reserveTable.addReserveWord("if", currentCode++);
	// entrypt is an attribute which can be attached to a function. Only one may be defined per assembly.
	// Indicates where the program should start in the bin.

	reserveTable.addReserveWord("entrypoint", currentCode++);
	types.addType("i8", 1, TypeCode::TYPE_INT8_CODE);
	types.addType("i16", 2, TypeCode::TYPE_INT16_CODE);
	types.addType("i32", 4, TypeCode::TYPE_INT32_CODE);
	types.addType("bool", 1, TypeCode::TYPE_BOOL_CODE);
	types.addType("float", 4, TypeCode::TYPE_FLOAT32_CODE);

	tokenCodeToOperator.emplace(PLUS_CODE, Operator::ADD);
	tokenCodeToOperator.emplace(MINUS_CODE, Operator::SUB);
	tokenCodeToOperator.emplace(STAR_CODE, Operator::MULT);
	tokenCodeToOperator.emplace(DIV_CODE, Operator::DIV);

	// Reserves 300 Series (functions)
	currentCode = 300;
	reserveTable.addReserveWord("fn", currentCode++);
}

Compiler::~Compiler()
{
}

bool Compiler::compileFile(const std::string& fileName, std::ostream& outputStream) {
	logger.setColorDisabled();
	logger.setPrefix("");
	// Collect the file size.
	FILE* file = fopen(fileName.c_str(), "rb");

	if (file != NULL) {
		fseek(file, 0L, SEEK_END);
		size_t fileSize = ftell(file);
		fseek(file, 0L, SEEK_SET);

		// Load the file from disk
		buffer = new char[fileSize + 1];
		fread(buffer, sizeof(char), fileSize, file);
		fclose(file);
		buffer[fileSize] = 0;
		bufferSize = (int)fileSize;
		currentLineNumber = 0;
		entryPoint = -1;

		currentBinaryOffset = 0;
		errorsFound = false;
		pass = 0;

		collectNextToken();
		parseFile();

		currentBinaryOffset = 0;
		currentLineNumber = 0;
		bufferIndex = 0;
		endOfParse = false;

		// If this happens, theres a bug.
		if (!symbolStack.atGlobalScope()) {
			addError("It appears a stack frame was not closed. This is a bug in the compiler.");
			errorsFound = true;
		}

		if (!errorsFound) {
			collectNextToken();
			pass = 1;
			parseFile();

			int outputBufferSize = currentBinaryOffset;
			currentBinaryOffset = 0;
			currentLineNumber = 0;
			bufferIndex = 0;
			endOfParse = false;
			pass = 2;

			if (!errorsFound) {
				collectNextToken();
				output = new char[outputBufferSize];
				parseFile();

				if (!errorsFound) {
					// WriteOBJ header
					writeOBJHeader(outputStream);
					// Write bin to the file.
					outputStream.write((char*)&outputBufferSize, sizeof(uint32_t));
					outputStream.write(output, outputBufferSize);
				}

				delete[] output;
			}
		}

		delete[] buffer;
	}
	else {
		logger.error("File could not be loaded from disk.");
		errorsFound = true;
	}

	return !errorsFound;
}

void Compiler::writeOBJHeader(std::ostream& output) {
	// The header should include the following information in this order:
	// if entry_point is specified 9 bytes for "########\0", 4 bytes offset
	// 4 bytes # of exported labels
	// List of exported labels and their respective positions in the file in this format
	//	"label_name\0", 4 bytes offset
	// 4 bytes # of imported labels
	// List of imported symbols followed by a list of locations where the byte should
	// Be substituted by the linker in this format:
	//	"label_name\0", 4 bytes number of subs, n bytes sub locations

	// Write the entry point to the file if it's specified.
	if (entryPoint != -1) {
		char booleanTrue = 1;
		output.write((char*)&booleanTrue, 1);
		output.write((char*)&entryPoint, sizeof(uint32_t));
	}
	else {
		char booleanFalse = 0;
		output.write((char*)&booleanFalse, 1);
	}

	// Write exported symbols to file.
	output.write((char*)&symbolStack.globalSymbols.exportedSymbolCount, sizeof(uint32_t));

	for (std::map<std::string, SymbolInfo*>::iterator i = symbolStack.globalSymbols.symbolInfo.begin(); 
		i != symbolStack.globalSymbols.symbolInfo.end(); ++i) {
		if (i->second->accessType == QueSymbolAccessType::EXPORTED) {
			output.write(i->first.c_str(), i->first.size() + 1);
			int value = i->second->value;
			output.write((char*)&value, sizeof(uint32_t));
		}
	}

	// Write imported symbols to file for the text segment.
	uint32_t textSegmentSymbolCount = symbolStack.globalSymbols.symbolImportReferencesTextSeg.size();
	output.write((char*)&textSegmentSymbolCount, sizeof(uint32_t));
	for (std::map<std::string, std::vector<int>>::iterator i = 
		symbolStack.globalSymbols.symbolImportReferencesTextSeg.begin(); 
		i != symbolStack.globalSymbols.symbolImportReferencesTextSeg.end(); ++i) 
	{
		std::vector<int>& references = i->second;
		int referenceCount = references.size();

		output.write(i->first.c_str(), i->first.size() + 1);

		// Write N occurrences
		output.write((char*)&referenceCount, sizeof(uint32_t));

		// Write offset to each occurence
		for (int i = 0; i < references.size(); i++) {
			output.write((char*)&references[i], sizeof(uint32_t));
		}
	}

	uint32_t dataSegmentSymbolCount = symbolStack.globalSymbols.symbolImportReferencesDataSeg.size();
	output.write((char*)&dataSegmentSymbolCount, sizeof(uint32_t));
	// Write the imported symbols for the data segment. (zeros will be fine because the correct value will be a direct substituation)
	for (std::map<std::string, std::vector<int>>::iterator i = 
		symbolStack.globalSymbols.symbolImportReferencesDataSeg.begin(); 
		i != symbolStack.globalSymbols.symbolImportReferencesDataSeg.end(); ++i) 
	{
		std::vector<int>& references = i->second;
		int referenceCount = references.size();

		output.write(i->first.c_str(), i->first.size() + 1);

		// Write N occurrences
		output.write((char*)&referenceCount, sizeof(uint32_t));

		// Write offset to each occurence
		for (int i = 0; i < references.size(); i++) {
			output.write((char*)&references[i], sizeof(uint32_t));
		}
	}

	// Write global offset table.
	uint32_t gotSize = symbolStack.globalSymbols.globalOffsetTable.size();
	output.write((char*)&gotSize, sizeof(uint32_t));
	// Write each occurrence to the file.
	for (int i = 0; i < gotSize; i++) {
		int location = symbolStack.globalSymbols.globalOffsetTable[i];
		output.write((char*)&location, sizeof(uint32_t));
	}
}

/// <summary>
/// At the file level, there can be the following in any order and in any quantity:
/// function defintion.
/// variable declaration.
/// </summary>
void Compiler::parseFile() {
	while (!isEndOfStream()) {
		bool needsSemi = true;
		int modifiers = 0;

		// Collect global modifiers.
		if (currentToken.code == reserveTable.getReserveCode("export")) {
			// Export can only be applied to external labels.
			modifiers |= EXPORT_FLAG;
			collectNextToken();
		}
		else if (currentToken.code == reserveTable.getReserveCode("import")) {
			modifiers |= EXTERN_FLAG;
			collectNextToken();
		}
		else if (currentToken.code == reserveTable.getReserveCode("entrypoint")) {
			modifiers |= ENTRYPT_FLAG;

			collectNextToken();
			if (currentToken.code == reserveTable.getReserveCode("fn")) {
				collectNextToken();
				SymbolInfo* info = new SymbolInfo();
				info->symbolType = QueSymbolType::FUNCTION;
				parseFunctionDefinition(modifiers, info);
				if (info->dimensions.size() != 0) {
					addError("Functions cannot return sized arrays");
				}
				continue;
			}
			else {
				addError("Expected function after entrypoint specifier");
				collectNextToken();
			}
		}

		if (currentToken.code == reserveTable.getReserveCode("constant")) {
			if (modifiers != 0) {
				addError("Illegal modifiers on constant");
			}

			collectNextToken();
			SymbolInfo* info = new SymbolInfo();
			info->symbolType = QueSymbolType::CONST_EXPR;
			parseConst(info, false);

			if (info->dimensions.size() != 0) {
				addError("Constants cannot be arrays");
			}
		}
		else if (currentToken.code == reserveTable.getReserveCode("var")) {
			collectNextToken();
			SymbolInfo* info = new SymbolInfo();
			info->symbolType = QueSymbolType::DATA;
			parseVariableDeclaration(modifiers, info);
		}
		else if (currentToken.code == reserveTable.getReserveCode("fn")) {
			collectNextToken();
			SymbolInfo* info = new SymbolInfo();
			info->symbolType = QueSymbolType::FUNCTION;
			parseFunctionDefinition(modifiers, info);

			if (info->dimensions.size() != 0) {
				addError("Functions cannot return sized arrays");
			}

			needsSemi = !info->defined;
		}
		else {
			addError("Unexpected token");
			break;
		}

		// A semi colon follows most global statements, however, some don't need one.
		if (needsSemi) {
			if (currentToken.code == SEMICOLON_CODE) {
				collectNextToken();
			}
			else {
				addError("Expected ';'");
			}
		}
	}
}

bool Compiler::getTypeFromToken(SymbolInfo& typeInfo) {
	bool foundType = false;
	typeInfo.typeInfo.ptrCount = 0;

	// TODO: support function ptr types.

	// Get the static type.
	if (types.typeExists(currentToken.lexeme)) {
		foundType = true;
		typeInfo.typeInfo.baseSize = types.getTypeSize(currentToken.lexeme);
		typeInfo.typeInfo.typeCode = types.getTypeCode(currentToken.lexeme);
	}

	collectNextToken();

	// Get ptr modifiers.
	while (currentToken.code == STAR_CODE && !isEndOfStream()) {
		typeInfo.typeInfo.ptrCount++;
		collectNextToken();
	}

	// Now, they can specify an N dimension array, and we don't really care how many dimensions it is.
	while (!isEndOfStream()) {
		if (currentToken.code == OPEN_BRACE_CODE) {
			typeInfo.symbolType = QueSymbolType::ARRAY;
			collectNextToken();
			ConstantValue size;
			// Each dimension of the array should be written to the binary.
			SymbolInfo* symbol = new SymbolInfo();
			symbol->typeInfo.baseSize = 4;
			symbol->typeInfo.ptrCount = 0;
			symbol->typeInfo.typeCode = TypeCode::TYPE_INT32_CODE;

			// I want the size of the array to be written into the things before the array
			// so the code can reference it with a keyword of somekind.
			parseConstantExpression(size, *symbol, false);

			int sizeInt = ((int*)&size.value)[0];

			if (sizeInt < 0) {
				addError("Array cannot have negative dimensions");
				collectNextToken();
				continue;
			}

			staticCast(size.type.typeCode, TypeCode::TYPE_INT32_CODE, size);
			typeInfo.dimensions.push_back(sizeInt);

			if (currentToken.code != CLOSE_BRACE_CODE) {
				addError("Expected ']'");
			}

			collectNextToken();
		}
		else {
			break;
		}
	}

	typeInfo.calculateTotalDimensionElements();

	return foundType;
}

/// <summary>
/// Here, we should be given a list of instructions that will finally be terminated
/// by a closing bracket.
/// </summary>
void Compiler::parseCodeSegment(SymbolInfo*& currentFunction) {
	symbolStack.pushScope();
	// Setup stack frame
	// We know the stack is already aligned because that's the caller's job
	// PUSH bp
	// mov bp, sp
	// push ra

	writeInstruction(encodeInstruction(PUSHW, false, RA));
	writeInstruction(encodeInstruction(PUSHW, false, BP));
	writeInstruction(encodeInstruction(MOV, false, BP, SP));

	while (!isEndOfStream()) {
		bool needsSemi = true;
		if (currentToken.code == reserveTable.getReserveCode("var")) {
			collectNextToken();
			SymbolInfo* info = new SymbolInfo();
			info->symbolType = QueSymbolType::DATA;
			parseVariableDeclaration(LOCAL_VAR_FLAG, info);
		}
		else if (currentToken.code == reserveTable.getReserveCode("return")) {
			collectNextToken();

			if (currentToken.code == SEMICOLON_CODE) {
			}
			else {
				SymbolInfo* info = new SymbolInfo();
				parseExpression(info);
				int reg = 0;
				int valueReg = symbolStack.allocateIntRegister(*info, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);
				writeInstruction(encodeInstruction(MOV, false, reg, valueReg));
				symbolStack.freeIntRegister(*info, false);
			}

			// Move the value into the return register

			// TODO: the function needs to remember it's return address between passes
			// so it can fill it in properly in pass 2/3
		}
		else if (currentToken.code == IDENTIFIER_CODE) {
			// It could either be an assignment or a function call
			SymbolInfo* value = symbolStack.searchSymbol(currentToken.lexeme);
			if (!value) {
				if (pass != 0) {
					addError("Undefined symbol: " + currentToken.lexeme);
				}
				collectNextToken();
				continue;
			}

			collectNextToken();

			if (currentToken.code == OPEN_PARN_CODE) {
				// Parse a function call
			}
			else if (currentToken.code == EQUALS_CODE) {
				// Parse variable assignment
				collectNextToken();
				SymbolInfo* info = new SymbolInfo();
				parseExpression(info);
				writeAssignment(*value, *info);
			}
		}
		else if (currentToken.code == reserveTable.getReserveCode("if")) {
			collectNextToken();
		}
		else if (currentToken.code == CLOSE_BRK_CODE) {
			collectNextToken();
			break;
		}
		else {
			addError("Unexpected token");
			collectNextToken();
			break;
		}

		// A semi colon follows most statements, however, some don't need one.
		if (needsSemi) {
			if (currentToken.code == SEMICOLON_CODE) {
				collectNextToken();
			}
			else {
				addError("Expected ';'");
			}
		}
	}

	// First restore the stackpointer.
	if (symbolStack.getCurrentBPOffset() != 0) {
		writeInstruction(encodeInstruction(ADD, true, SP, SP, 0), true, symbolStack.getCurrentBPOffset());
	}

	// pop bp
	// mov sp, bp
	writeInstruction(encodeInstruction(POPW, false, BP));
	writeInstruction(encodeInstruction(POPW, false, RA));

	symbolStack.popScope();
}

/// <summary>
/// Going to consume tokens until we have reached the end of the argument list and the parentheses 
/// have been closed.
/// </summary>
/// <param name="name"></param>
/// <param name="returnType"></param>
/// <param name="args"></param>
/// <returns></returns>
void Compiler::parseFunctionDefinition(int modifiers, SymbolInfo*& functionSymbol) {
	// The first expectation is an open parentheses in which the arguments will be enumerated.
	bool success = false;
	symbolStack.pushScope();
	std::vector<SymbolInfo*> arguments;
	
	// addRet is set to false if and only if we are in an entrypoint. Ret is replaced with a program halt swi
	bool addRet = true;

	// Make sure we are not adding more than one entrypoint
	if (modifiers & ENTRYPT_FLAG) {
		if (pass == 2) {
			if (entryPoint == -1) {
				entryPoint = currentBinaryOffset;
			}
			else {
				collectNextToken();
				addError("Entrypoint defined multiple times");
			}
		}

		addRet = false;
	}

	if (currentToken.code == OPEN_PARN_CODE) {
		collectNextToken();

		// Collect arguments from set.
		// TODO somehow set offsets for arguments
		while (!isEndOfStream()) {
			int argModifiers = ARGUMENT_FLAG;

			if (currentToken.code == IDENTIFIER_CODE) {
				arguments.push_back(new SymbolInfo());
				parseVariableDeclaration(argModifiers, arguments[arguments.size() - 1]);
			}
			else if (currentToken.code == CLOSE_PARN_CODE) {
				// Close paren escapes the argument list.
				success = true;
				collectNextToken();
				break;
			}
			else {
				addError("Expected ')'");
				collectNextToken();
				break;
			}

			if (currentToken.code == COMMA_CODE) {
				collectNextToken();
			}
		}
	}
	else {
		addError("Expected '('");
		collectNextToken();
	}

	// Parse remainder of function.
	// Should be the same syntax as a normal variable declaration
	if (success) {
		if (currentToken.code == IDENTIFIER_CODE) {
			// Have to do something weird to make sure this function get's written to the previous scope.
			// Go back down to the previous scope, parse the var declaration, then
			// return back to this scope.

			symbolStack.enterPreviousScope();
			parseVariableDeclaration(modifiers, functionSymbol, &arguments);
			symbolStack.enterCurrentScope();

			// ... parse code block
			if (currentToken.code == OPEN_BRK_CODE) {
				collectNextToken();
				
				// Function return address stores the place to jump to when return is called.
				parseCodeSegment(functionSymbol);

				// ret = jmp ra
				if (addRet) {
					writeInstruction(encodeInstruction(JMP, false, RA));
				}
				else {
					// Or terminate the program if that's what we have to do
					writeInstruction(encodeInstruction(SWI, true, 0), true, 0x0);
				}
				functionSymbol->defined = true;
				// The caller must remove the parameters from stack. I ain't doin it here.
			}
			else {
				// We're just a function declaration, move along.
				if (modifiers & EXTERN_FLAG) {
					functionSymbol->defined = false;
				}
				else {
					addError("Declaration not legal on non-extern function");
				}
			}
		}
		else {
			success = false;
			addError("Expected identifier for function name");
			collectNextToken();
		}
	}

	// We need to know where the code to escape the function is in case they call return early.
	symbolStack.popScope();
}

void Compiler::parseConst(SymbolInfo*& symbolInfo, bool allowRefs) {
	if (currentToken.code == IDENTIFIER_CODE) {
		symbolInfo->name = currentToken.lexeme;
		collectNextToken();
		bool success = true;

		if (currentToken.code == COLON_CODE) {
			collectNextToken();

			// Get the variable type.
			if (getTypeFromToken(*symbolInfo)) {
			}
			else {
				addError("Type not found in this scope");
				collectNextToken();
			}
		}
		else {
			addError("Expected ':'");
			success = false;
		}

		if (success) {
			// Record the variable in the current frame.
			symbolInfo->accessType = QueSymbolAccessType::COMPILER_ACCESS;
			symbolInfo->symbolType = QueSymbolType::CONST_EXPR;

			if (currentToken.code == EQUALS_CODE) {
				collectNextToken();
				ConstantValue value;
				parseConstantExpression(value, *symbolInfo, allowRefs);

				// Cast the value to the proper resulting type, then write it to the data segment
				symbolStack.addSymbolToCurrentScope(symbolInfo);
				staticCast(value.type.typeCode, symbolInfo->typeInfo.typeCode, value);
				symbolInfo->value = *(uint32_t*)&value.value;
				//symbolStack.globalSymbols.setSymbolValue(symbolInfo.name, symbolInfo.value);
				symbolInfo->defined = true;
				//symbolStack.globalSymbols.setSymbolDefined(symbolInfo.name);
			}
			else {
				addError("Contants must be assigned an initial value");
				collectNextToken();
			}

			return;
		}
	}
	else {
		addError("Expected name");
	}

	collectNextToken();
}

// A variable declaration consists of a name, followed by a colon
// then a type value and an optional equals
// Function is responsible for getting the variable name and typecode
void Compiler::parseVariableDeclaration(int modifiers, SymbolInfo*& symbolInfo, std::vector<SymbolInfo*>* args) {
	if (args != nullptr) {
		symbolInfo->fnArgs = *args;
	}

	if (currentToken.code == IDENTIFIER_CODE) {
		symbolInfo->name = currentToken.lexeme;
		collectNextToken();
		bool success = true;

		if (currentToken.code == COLON_CODE) {
			collectNextToken();

			// Get the variable type.
			if (getTypeFromToken(*symbolInfo)) {
			}
			else {
				addError("Type not found in this scope");
				collectNextToken();
			}
		}
		else if (symbolInfo->symbolType == QueSymbolType::FUNCTION) {
		}
		else {
			addError("Expected ':'");
			success = false;
		}

		if (success) {
			// Record the variable in the current frame.
			getSymbolInfo(modifiers, *symbolInfo);

			if (!errorsFound) {
				// If we couldn't add it to the scope and we are on the first pass, the symbol was declared
				// multiple times.
				if (!symbolStack.addSymbolToCurrentScope(symbolInfo) && pass < 1) {
					addError("Symbol defined multiple times: " + symbolInfo->name);
				}
			}
			else {
				return;
			}

			// Allocate space for variable, whether we are in the data segment, args, or local variables,
			// We can handle it here.
			symbolStack.pushVariable(*symbolInfo);

			// Handle initialization, if its a variable (data or local)
			if ((symbolInfo->accessType == QueSymbolAccessType::LOCAL ||
				symbolInfo->accessType == QueSymbolAccessType::EXPORTED)) 
			{
				// Handle data varaible
				if (symbolInfo->symbolType == QueSymbolType::DATA) {
					if (currentToken.code == EQUALS_CODE) {
						collectNextToken();
						ConstantValue value;
						parseConstantExpression(value, *symbolInfo, true);

						// Cast the value to the proper resulting type, then write it to the data segment
						writeConstantValueToDataSegment(value, *symbolInfo);
					}
				} // Normal global
				else if (symbolInfo->symbolType == QueSymbolType::ARRAY) {
					if (currentToken.code == EQUALS_CODE) {
						collectNextToken();
						// Parse array initializer

						if (currentToken.code == OPEN_BRK_CODE) {
							collectNextToken();
							parseConstantArrayInitializer(*symbolInfo, 0, 0);

							if (currentToken.code != CLOSE_BRK_CODE) {
								addError("Expected '}'");
							}

							collectNextToken();
						}
						else {
							addError("Expected array initializer token: '{'");
							collectNextToken();
						}
					}
				} // Array
			}
			else if (symbolInfo->accessType == QueSymbolAccessType::VARIABLE) {
				if (currentToken.code == EQUALS_CODE) {
					collectNextToken();
					// Handle local variable
					if (symbolInfo->symbolType == QueSymbolType::DATA) {
						SymbolInfo* assignment = new SymbolInfo();
						parseExpression(assignment);
						writeAssignment(*symbolInfo, *assignment);
					} // Normal scoped local variable
					// TODO: arrays
				}
			}

			return;
		}
	}
	else {
		addError("Expected variable name");
	}

	collectNextToken();
}

void Compiler::getSymbolInfo(int modifiers, SymbolInfo& info) {
	// What kind of variable are we dealing with?
	if (symbolStack.atGlobalScope()) {
		if (modifiers & EXTERN_FLAG) {
			info.accessType = QueSymbolAccessType::IMPORTED;
		}
		else if (modifiers & EXPORT_FLAG) {
			info.accessType = QueSymbolAccessType::EXPORTED;
		}
		else {
			info.accessType = QueSymbolAccessType::LOCAL;
		}

		info.value = currentBinaryOffset;
	}
	else {
		// Can't yet assign offsets to args and locals because it's based on
		// The stack ptr.
		if (modifiers & EXTERN_FLAG || modifiers & EXPORT_FLAG) {
			addError("Invalid modifier for local variable");
		}

		if (modifiers & ARGUMENT_FLAG) {
			info.accessType = QueSymbolAccessType::ARGUMENT;
		}
		else if (modifiers & LOCAL_VAR_FLAG) {
			info.accessType = QueSymbolAccessType::VARIABLE;
		}
		else if (info.symbolType == QueSymbolType::FUNCTION) {
			info.accessType = QueSymbolAccessType::LOCAL;
			info.value = currentBinaryOffset;
		}
		else {
			addError("Undefined symbol type");
		}
	}
}

bool Compiler::readComment() {
	// Enter into a comment till the end of the line
	if (buffer[bufferIndex] == '/' && buffer[bufferIndex + 1] == '/') {
		while (!isEndOfStream() && buffer[bufferIndex] != '\n') {
			bufferIndex++;
		}

		return true;
	}

	return false;
}

bool Compiler::readOneTwoChar() {
	if (buffer[bufferIndex] == ',') {
		currentToken.lexeme = ",";
		currentToken.code = COMMA_CODE;
	}
	else if (buffer[bufferIndex] == '>' && buffer[bufferIndex + 1] == '=') {
		currentToken.lexeme = ">=";
		currentToken.code = GTE_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '<' && buffer[bufferIndex + 1] == '=') {
		currentToken.lexeme = "<=";
		currentToken.code = LTE_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '=' && buffer[bufferIndex + 1] == '=') {
		currentToken.lexeme = "==";
		currentToken.code = ARE_EQUAL_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '!' && buffer[bufferIndex + 1] == '=') {
		currentToken.lexeme = "!=";
		currentToken.code = NE_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '>') {
		currentToken.lexeme = '>';
		currentToken.code = GT_CODE;
	}
	else if (buffer[bufferIndex] == '<') {
		currentToken.lexeme = '<';
		currentToken.code = LT_CODE;
	}
	else if (buffer[bufferIndex] == ':') {
		currentToken.lexeme = ":";
		currentToken.code = COLON_CODE;
	}
	else if (buffer[bufferIndex] == ';') {
		currentToken.lexeme = ";";
		currentToken.code = SEMICOLON_CODE;
	}
	else if (buffer[bufferIndex] == '*') {
		currentToken.lexeme = "*";
		currentToken.code = STAR_CODE;
	}
	else if (buffer[bufferIndex] == '(') {
		currentToken.lexeme = "(";
		currentToken.code = OPEN_PARN_CODE;
	}
	else if (buffer[bufferIndex] == ')') {
		currentToken.lexeme = ")";
		currentToken.code = CLOSE_PARN_CODE;
	}
	else if (buffer[bufferIndex] == '{') {
		currentToken.lexeme = "{";
		currentToken.code = OPEN_BRK_CODE;
	}
	else if (buffer[bufferIndex] == '}') {
		currentToken.lexeme = "}";
		currentToken.code = CLOSE_BRK_CODE;
	}
	else if (buffer[bufferIndex] == '=') {
		currentToken.lexeme = "=";
		currentToken.code = EQUALS_CODE;
	}
	else if (buffer[bufferIndex] == '-') {
		currentToken.lexeme = "-";
		currentToken.code = MINUS_CODE;
	}
	else if (buffer[bufferIndex] == '+') {
		currentToken.lexeme = "+";
		currentToken.code = PLUS_CODE;
	}
	else if (buffer[bufferIndex] == '/') {
		currentToken.lexeme = "/";
		currentToken.code = DIV_CODE;
	}
	else if (buffer[bufferIndex] == '[') {
		currentToken.lexeme = '[';
		currentToken.code = OPEN_BRACE_CODE;
	}
	else if (buffer[bufferIndex] == ']') {
		currentToken.lexeme = ']';
		currentToken.code = CLOSE_BRACE_CODE;
	}
	else if (buffer[bufferIndex] == '%') {
		currentToken.lexeme = '%';
		currentToken.code = MODULUS_CODE;
	}
	else {
		return false;
	}

	return true;
}

void SymbolStack::pushVariable(SymbolInfo& symbol) {
	// Compute total number of elements, 1 if it's not an array
	uint64_t size = symbol.calculateTotalSize();
	size = (size + 3) & 0xFFFFFFFC;

	if (size > INT32_MAX) {
		compiler->addError("Array surpasses max size of: 2Gb");
		return;
	}

	if (symbol.symbolType == QueSymbolType::FUNCTION ||
		symbol.accessType == QueSymbolAccessType::ARGUMENT ||
		symbol.accessType == QueSymbolAccessType::IMPORTED)
	{
	}
	else if (symbol.accessType == QueSymbolAccessType::EXPORTED || symbol.accessType == QueSymbolAccessType::LOCAL) {
		compiler->writeSymbolToDataSegment(size, symbol);
	}
	else if (symbol.accessType == QueSymbolAccessType::VARIABLE) {
		assert(!atGlobalScope());
		localScope[scopeIndex]->pushLocalVariable(compiler, size);
		symbol.value = localScope[scopeIndex]->currentOffsetFromBP;
		//localScope[scopeIndex]->setSymbolValue(symbol.name, symbol.value);
	}
	else if (symbol.accessType == QueSymbolAccessType::COMPILER_ACCESS) {
		// We don't need to do anything.
	}
	else {
		std::cout << "UNHANDLED VARIABLE HERE\n";
	}
}

void LocalSymbolTable::pushLocalVariable(Compiler* compiler, int size) {
	compiler->writeInstruction(encodeInstruction(SUB, true, SP, SP, 0), true, size);
	currentOffsetFromBP += size;
}

bool Compiler::isAddop() {
	if (currentToken.code == PLUS_CODE || currentToken.code == MINUS_CODE) {
		return true;
	}
	return false;
}

bool Compiler::isMulop() {
	if (
		currentToken.code == STAR_CODE || 
		currentToken.code == DIV_CODE || 
		currentToken.code == MODULUS_CODE
		) {
		return true;
	}
	return false;
}

bool Compiler::isCompareOp() {
	if (currentToken.code == GT_CODE) {
	}
	else if (currentToken.code == GTE_CODE) {
	}
	else if (currentToken.code == LT_CODE) {
	}
	else if (currentToken.code == LTE_CODE) {
	}
	else if (currentToken.code == ARE_EQUAL_CODE) {
	}
	else if (currentToken.code == NE_CODE) {
	}
	else {
		return false;
	}

	return true;
}

void Compiler::parseMulOp(Operator& op) {
	if (currentToken.code == STAR_CODE) {
		op = Operator::MULT;
	}
	else if (currentToken.code == DIV_CODE) {
		op = Operator::DIV;
	}
	else if (currentToken.code == MODULUS_CODE) {
		op = Operator::MOD;
	}
	else {
		addError("Expected mul operator");
	}

	collectNextToken();
}
void Compiler::parseAddOp(Operator& op) {
	if (currentToken.code == PLUS_CODE) {
		op = Operator::ADD;
	}
	else if (currentToken.code == MINUS_CODE) {
		op = Operator::SUB;
	}
	else {
		addError("Expected add operator");
	}

	collectNextToken();
}

void Compiler::parseCompareOp(Operator& op) {
	if (currentToken.code == GT_CODE) {
		op = Operator::CMP_GT;
	}
	else if (currentToken.code == GTE_CODE) {
		op = Operator::CMP_GTE;
	}
	else if (currentToken.code == LT_CODE) {
		op = Operator::CMP_LT;
	}
	else if (currentToken.code == LTE_CODE) {
		op = Operator::CMP_LTE;
	}
	else if (currentToken.code == ARE_EQUAL_CODE) {
		op = Operator::CMP_EQ;
	}
	else if (currentToken.code == NE_CODE) {
		op = Operator::CMP_NE;
	}
	else {
		addError("Expected comparison operator");
	}

	collectNextToken();
}
