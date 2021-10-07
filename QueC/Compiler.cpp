#include "Compiler.h"
#include "../QueASM/Assembler.h"

#define ARG_STACK_BP_OFFSET_START 12

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
	reserveTable.addReserveWord("else", currentCode++);
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

/// <summary>
/// At the file level, there can be the following in any order and in any quantity:
/// function defintion.
/// variable declaration.
/// </summary>
void Compiler::parseFile() {
	symbolStack.temporaryVariableIdx = 0;
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
				SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
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
			SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
			info->symbolType = QueSymbolType::CONST_EXPR;
			parseConst(info, false);

			if (info->dimensions.size() != 0) {
				addError("Constants cannot be arrays");
			}
		}
		else if (currentToken.code == reserveTable.getReserveCode("var")) {
			collectNextToken();
			SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
			info->symbolType = QueSymbolType::DATA;
			parseVariableDeclaration(modifiers, info);
		}
		else if (currentToken.code == reserveTable.getReserveCode("fn")) {
			collectNextToken();
			SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
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
			SymbolInfo* symbol = new SymbolInfo(symbolStack.scopeIndex);
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
int Compiler::parseCodeSegment(SymbolInfo*& currentFunction, int scopeLevel, int scopeReturnAddress) {
	// Add a return point to this stackframe.
	if (scopeLevel + 1 > (int)currentFunction->functionReturnPointers.size()) {
		currentFunction->functionReturnPointers.push_back(std::vector<int>());
		currentFunction->functionStackFrameSizes.push_back(std::vector<int>());
	}
	int subStackCount = 0;

	while (!isEndOfStream()) {
		bool needsSemi = true;
		if (currentToken.code == reserveTable.getReserveCode("var")) {
			collectNextToken();
			SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
			info->symbolType = QueSymbolType::DATA;
			parseVariableDeclaration(LOCAL_VAR_FLAG, info);
		}
		else if (currentToken.code == reserveTable.getReserveCode("if")) {
			collectNextToken();
			parseIfStatement(currentFunction, scopeLevel, scopeReturnAddress, subStackCount);
			needsSemi = false;
		}
		else if (currentToken.code == reserveTable.getReserveCode("return")) {
			collectNextToken();

			if (currentToken.code == SEMICOLON_CODE) {
			}
			else {
				SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
				parseExpression(info);
				int reg = 0;
				int valueReg = symbolStack.allocateIntRegister(*info, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);
				writeInstruction(encodeInstruction(MOV, false, reg, valueReg));
				symbolStack.freeIntRegister(*info, false);
			}

			writeInstruction(encodeInstruction(JMP, true), true,
				currentFunction->functionReturnPointer - (currentBinaryOffset + 4ULL));
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
				collectNextToken();
				if (value->symbolType != QueSymbolType::FUNCTION) {
					addError(value->fileName + " is not a function");
					continue;
				}
				// Parse a function call
				SymbolInfo* fnRet = new SymbolInfo(symbolStack.scopeIndex);
				parseFunctionCall(value, fnRet);
				symbolStack.freeIntRegister(*fnRet, false);
			}
			else if (currentToken.code == EQUALS_CODE) {
				// Parse variable assignment
				collectNextToken();
				SymbolInfo* info = new SymbolInfo(symbolStack.scopeIndex);
				parseExpression(info);
				writeAssignmentInstructions(*value, *info);
			}
		}
		else if (currentToken.code == OPEN_BRK_CODE) {
			setupBlockStackframe(currentFunction, scopeLevel, scopeReturnAddress, subStackCount);
			subStackCount++;
			needsSemi = false;
		}
		else if (currentToken.code == CLOSE_BRK_CODE) {
			collectNextToken();
			break;
		}
		else if (currentToken.code == SEMICOLON_CODE) {
			collectNextToken();
			needsSemi = false;
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

	return 0;
}

/// <summary>
/// Here, we should be looking at N comma delimiated expressions. Each one is attached 
/// in order to a SymbolInfo* stored in the function call's scope. So all I have to do is 
/// calculate each of those arguments, store them on stack by saving them then unconditonally jump into
/// to function call. Every argument should be copied onto the stack just waiting for us to take them off 
/// and use them! Remember that these arguments are not a part of the previous stack frame, so it's
/// the responsbility of the caller to restore the stack to the state before the fn call.
/// </summary>
/// <param name="ret"></param>
void Compiler::parseFunctionCall(SymbolInfo*& functionSymbol, SymbolInfo*& ret) {
	if (functionSymbol->typeInfo.baseSize == 0) {
		addError("Funcitons must return a value to be used as an R value");
	}

	ret->accessType = QueSymbolAccessType::VARIABLE;
	ret->symbolType = QueSymbolType::DATA;
	ret->isGlobal = false;
	ret->isTemporaryValue = false;
	ret->typeInfo = functionSymbol->typeInfo;
	ret->fileName = functionSymbol->fileName;

	// Push all active registers to the stacc.
	for (int i = 0; i < symbolStack.registerInUse.size(); i++) {
		writeInstruction(encodeInstruction(PUSHW, false, symbolStack.registerInUse[i]));
	}	
	
	// Preserve the compiler state of the registers because messing with args will push vars in and out of registers.
	std::map<std::string, Register> previousRegAllocStack = symbolStack.registerAllocationStack;
	std::vector<Register> previousRegisterInUse = symbolStack.registerInUse;
	std::stack<Register> previousAvailableRegisters = symbolStack.availableRegisters;

	// Now we basically have a completely free register state, let's take each variable and
	// put them in a register, then push them in the opposite order.
	std::vector<SymbolInfo*> args;

	// Set the base ptr offset to point to the things right before ra, sp, bp
	while (currentToken.code != CLOSE_PARN_CODE) {
		SymbolInfo* exprSymbol = new SymbolInfo(symbolStack.scopeIndex);
		parseExpression(exprSymbol);
		args.push_back(exprSymbol);

		int reg = symbolStack.allocateIntRegister(*exprSymbol, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);

		if (currentToken.code != COMMA_CODE) {
			break;
		}
		else {
			collectNextToken();
		}
	}

	if (currentToken.code == CLOSE_PARN_CODE) {
		collectNextToken();
	}
	else {
		addError("Expected ')'");
		collectNextToken();
	}

	// Basic argument count semantic checking, Idc about the type at this point.
	if (args.size() > functionSymbol->fnArgs.size()) {
		addError("Too many arguments for: " + functionSymbol->fileName);
	}
	else if (args.size() < functionSymbol->fnArgs.size()) {
		addError("Too few arguments for: " + functionSymbol->fileName);
	}

	// Push each argument to stacc.
	// If we accidentally push things the funciton needs out of the register state, oh well :shrug:
	for (int i = args.size() - 1; i >= 0; i--) {
		// Unless there are a lot of args, most of these should already be in registers.
		int reg = symbolStack.allocateIntRegister(*args[i], RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);
		// For now, each and every operand is 4 bytes, so this doesn't have to be that complex.
		writeInstruction(encodeInstruction(PUSHW, false, reg));
		symbolStack.freeIntRegister(*args[i], false);
	}

	// Now that all the args have been pushed, let's restore the compiler register state.
	symbolStack.registerAllocationStack = previousRegAllocStack;
	symbolStack.registerInUse = previousRegisterInUse;
	symbolStack.availableRegisters = previousAvailableRegisters;
	//									!!!!ATTENTION!!!!
	// --- THE REGISTER STATE SHOULD **ABSOLUTELY** NOT BE MODIFIED TILL THE ARGS ARE POPPED OFF.
	// --- IF YOU'RE MODIFYING THE STATE BEFORE THEN, YOU ARE INTRODUCING AN EXTREMELY HARD TO FIND BUG.
	//									!!!!ATTENTION!!!!

	if (functionSymbol->accessType == QueSymbolAccessType::IMPORTED) {
		// TODO: handle imported
	}
	else {
		writeInstruction(encodeInstruction(CALL, true), true, 
			functionSymbol->value - (currentBinaryOffset + 4ULL));

		// Restore the stacc after pushing args
		if (args.size() != 0) {
			writeInstruction(encodeInstruction(ADD, true, SP, SP), true, args.size() * 4);
		}

		// Pop off allocated registers to restore their state.
		for (int i = symbolStack.registerInUse.size() - 1; i >= 0; i--) {
			writeInstruction(encodeInstruction(POPW, false, symbolStack.registerInUse[i]));
		}
	}

	// Move the value from R0, to the return variable.
	int destReg = symbolStack.allocateIntRegister(*ret, RegisterAllocMode::DONT_LOAD);
	writeInstruction(encodeInstruction(MOV, false, destReg, R0));

	//symbolStack.freeIntRegister(*ret, true);
}

// An if statement parses an expression and compares the result to zero. If its zero, the condition returns false,
// and it jumps around the if execution block, otherwise it's true and it continues execution only to
// jump around the code that would have jumped out afterwards. Simple stuff
void Compiler::parseIfStatement(SymbolInfo*& currentFunctions, int scopelevel, int scopeReturnAddress, int& subStackIndex) {
	SymbolInfo* temporarySymbol = new SymbolInfo(symbolStack.scopeIndex);
	parseExpression(temporarySymbol);
	int reg = symbolStack.allocateIntRegister(*temporarySymbol, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);
	writeInstruction(encodeInstruction(CMP, true, reg), true, 0);
	//symbolStack.freeIntRegister(*temporarySymbol, true);

	// Setup location to jump to
	int falseJumpLblOffset = currentBinaryOffset + 4;
	writeInstruction(encodeInstruction(JE, true), true, 0);

	setupBlockStackframe(currentFunctions, scopelevel, scopelevel, subStackIndex);
	subStackIndex++;

	// Should jump around the stack frame if false, so update the label here.
	fillInMissingImmediate(falseJumpLblOffset, currentBinaryOffset - falseJumpLblOffset);

	if (currentToken.code == reserveTable.getReserveCode("else")) {
		collectNextToken();
		reg = symbolStack.allocateIntRegister(*temporarySymbol, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);
		writeInstruction(encodeInstruction(CMP, true, reg), true, 1);
		falseJumpLblOffset = currentBinaryOffset + 4;
		writeInstruction(encodeInstruction(JE, true), true, 0);

		if (currentToken.code == reserveTable.getReserveCode("if")) {
			collectNextToken();
			parseIfStatement(currentFunctions, scopelevel, scopeReturnAddress, subStackIndex);
			subStackIndex++;
		}
		else if (currentToken.code == OPEN_BRK_CODE) {
			setupBlockStackframe(currentFunctions, scopelevel, scopeReturnAddress, subStackIndex);
			subStackIndex++;
		}
		else {
			addError("Unexpected token");
		}

		// Fill in true jump around else
		fillInMissingImmediate(falseJumpLblOffset, currentBinaryOffset - falseJumpLblOffset);
	}

	symbolStack.freeIntRegister(*temporarySymbol, false);
}

void Compiler::setupBlockStackframe(SymbolInfo*& currentFunction, int scopeLevel, int scopeReturnAddress, int subStackCount) {
	int addr = 0;
	int stackFrameSize = 0;
	if (pass != 0) {
		addr = currentFunction->functionReturnPointers[scopeLevel][subStackCount];
		stackFrameSize = currentFunction->functionStackFrameSizes[scopeLevel][subStackCount];
	}

	// Make space for the variables
	writeInstruction(encodeInstruction(SUB, true, SP, SP, 0), true, stackFrameSize);

	collectNextToken();
	symbolStack.pushScope();
	symbolStack.setFrameSize(stackFrameSize);
	int retPopStk = parseCodeSegment(currentFunction, scopeLevel + 1, addr);

	// Normal stack frame exit: fix SP, jump past the stack frame terminator

	if (pass == 0) {
		currentFunction->functionReturnPointers[scopeLevel].push_back(currentBinaryOffset);
		currentFunction->functionStackFrameSizes[scopeLevel].push_back(symbolStack.getCurrentBPOffset());
	}
	else {
		currentFunction->functionReturnPointers[scopeLevel][subStackCount] = currentBinaryOffset;
		currentFunction->functionStackFrameSizes[scopeLevel][subStackCount] = symbolStack.getCurrentBPOffset();
	}

	subStackCount++;

	// First restore the stackpointer.
	writeInstruction(encodeInstruction(ADD, true, SP, SP, 0), true, stackFrameSize);
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

	if (currentToken.code == IDENTIFIER_CODE) {
		// Have to do something weird to make sure this function get's written to the previous scope.
		// Go back down to the previous scope, parse the var declaration, then
		// return back to this scope.

		symbolStack.enterPreviousScope();
		parseVariableDeclaration(modifiers, functionSymbol);
		symbolStack.enterCurrentScope();
		success = true;
	}
	else {
		success = false;
		addError("Expected identifier for function name");
		collectNextToken();
	}

	if (success && currentToken.code == OPEN_PARN_CODE) {
		collectNextToken();

		// Collect arguments from set.
		// TODO somehow set offsets for arguments
		int argIndex = 0;
		while (!isEndOfStream()) {
			int argModifiers = ARGUMENT_FLAG;

			if (currentToken.code == IDENTIFIER_CODE) {
				SymbolInfo* nextArgument;

				nextArgument = new SymbolInfo(symbolStack.scopeIndex);
				nextArgument->symbolType = QueSymbolType::DATA;
				functionSymbol->fnArgs.push_back(nextArgument);

				parseVariableDeclaration(argModifiers, 
					functionSymbol->fnArgs[functionSymbol->fnArgs.size() - 1]);
				nextArgument->value = -(ARG_STACK_BP_OFFSET_START + (4 * argIndex));
				argIndex++;
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

	if (success) {
		// ... parse code block
		if (currentToken.code == OPEN_BRK_CODE) {
			collectNextToken();
			symbolStack.pushScope();
			symbolStack.setFrameSize(functionSymbol->baseStackFrameSize);
			// Setup stack frame
			// We know the stack is already aligned because that's the caller's job
			// PUSH bp
			// mov bp, sp
			// push ra

			writeInstruction(encodeInstruction(PUSHW, false, RA));
			writeInstruction(encodeInstruction(PUSHW, false, BP));
			writeInstruction(encodeInstruction(PUSHW, false, SP));
			writeInstruction(encodeInstruction(MOV, false, BP, SP));

			// Subtract stack space for the variables
			writeInstruction(encodeInstruction(SUB, true, SP, SP), true, functionSymbol->baseStackFrameSize);

			// Function return address stores the place to jump to when return is called.
			parseCodeSegment(functionSymbol, 0, functionSymbol->functionReturnPointer);

			functionSymbol->functionReturnPointer = currentBinaryOffset;
			functionSymbol->baseStackFrameSize = symbolStack.getCurrentBPOffset();

			// First restore the stackpointer.
			writeInstruction(encodeInstruction(LW, false, SP, BP));

			// pop bp
			// mov sp, bp
			writeInstruction(encodeInstruction(POPW, false, BP));
			writeInstruction(encodeInstruction(POPW, false, RA));

			symbolStack.popScope();

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
void Compiler::parseVariableDeclaration(int modifiers, SymbolInfo*& symbolInfo) {
	if (currentToken.code == IDENTIFIER_CODE) {
		symbolInfo->name = currentToken.lexeme;
		symbolInfo->fileName = currentToken.lexeme;
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
						SymbolInfo* assignment = new SymbolInfo(symbolStack.scopeIndex);
						parseExpression(assignment);
						writeAssignmentInstructions(*symbolInfo, *assignment);
						symbolStack.freeIntRegister(*assignment, false);
					} // Normal scoped local variable
					else if (symbolInfo->symbolType == QueSymbolType::ARRAY) {
						if (currentToken.code == OPEN_BRK_CODE) {
							collectNextToken();
							SymbolInfo* variableSymbol = new SymbolInfo(symbolStack.scopeIndex);
							parseArrayInitializer(symbolInfo, variableSymbol, 0, 0);
							symbolStack.freeIntRegister(*variableSymbol, false);

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
			info.accessType = QueSymbolAccessType::VARIABLE;
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
	else if (buffer[bufferIndex] == '~') {
		currentToken.lexeme = '~';
		currentToken.code = COMP_CODE;
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
	else if (buffer[bufferIndex] == '&' && buffer[bufferIndex + 1] == '&') {
		currentToken.lexeme = "&&";
		currentToken.code = AND_AND_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '|' && buffer[bufferIndex + 1] == '|') {
		currentToken.lexeme = "||";
		currentToken.code = OR_OR_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '<' && buffer[bufferIndex + 1] == '<') {
		currentToken.lexeme = "<<";
		currentToken.code = LSL_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '>' && buffer[bufferIndex + 1] == '>') {
		currentToken.lexeme = ">>";
		currentToken.code = LSR_CODE;
		bufferIndex++;
	}
	else if (buffer[bufferIndex] == '&') {
		currentToken.lexeme = '&';
		currentToken.code = AND_CODE;
	}
	else if (buffer[bufferIndex] == '|') {
		currentToken.lexeme = '|';
		currentToken.code = OR_CODE;
	}
	else if (buffer[bufferIndex] == '^') {
		currentToken.lexeme = '^';
		currentToken.code = XOR_CODE;
	}
	else if (buffer[bufferIndex] == '!') {
		currentToken.lexeme = '!';
		currentToken.code = NOT_CODE;
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
		symbol.stackFrameIndex = scopeIndex;
		localScope[scopeIndex]->pushLocalVariable(compiler, size);
		symbol.value = localScope[scopeIndex]->getLocalBPOffset();
	}
	else if (symbol.accessType == QueSymbolAccessType::COMPILER_ACCESS) {
		// We don't need to do anything.
	}
	else {
		std::cout << "UNHANDLED VARIABLE HERE\n";
	}
}

void LocalSymbolTable::pushLocalVariable(Compiler* compiler, int size) {
	//compiler->writeInstruction(encodeInstruction(SUB, true, SP, SP, 0), true, size);
	currentOffsetFromBP += size;
}

bool Compiler::isPrefixOp() {
	if (currentToken.code == NOT_CODE ||
		currentToken.code == MINUS_CODE ||
		currentToken.code == COMP_CODE
		) {
	}
	else {
		return false;
	}

	return true;
}

void Compiler::parsePrefixOp(Operator& op) {
	if (currentToken.code == NOT_CODE) {
		op = Operator::NOT;
	}
	else if (currentToken.code == MINUS_CODE) {
		op = Operator::NEGATE;
	}
	else if (currentToken.code == COMP_CODE) {
		op = Operator::COMPLEMENT;
	}
	else {
		addError("Expected a prefix operator");
	}

	collectNextToken();
}

bool Compiler::isLogicalOp() {
	if (currentToken.code == AND_AND_CODE) {
	}
	else if (currentToken.code == OR_OR_CODE) {
	}
	else {
		return false;
	}
	return true;
}

void Compiler::parseLogicalOp(Operator& op) {
	if (currentToken.code == AND_AND_CODE) {
		op = Operator::AND_AND;
	}
	else if (currentToken.code == OR_OR_CODE) {
		op = Operator::OR_OR;
	}
	else {
		addError("Expected logical operator");
	}

	collectNextToken();
}

bool Compiler::isAddop() {
	if (currentToken.code == PLUS_CODE ||
		currentToken.code == MINUS_CODE ||
		currentToken.code == AND_CODE ||
		currentToken.code == OR_CODE
		) {
	}
	else {
		return false;
	}
	return true;
}

bool Compiler::isMulop() {
	if (
		currentToken.code == STAR_CODE ||
		currentToken.code == DIV_CODE ||
		currentToken.code == MODULUS_CODE ||
		currentToken.code == XOR_CODE ||
		currentToken.code == LSL_CODE ||
		currentToken.code == LSR_CODE
		) {
		return true;
	}
	return false;
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
	else if (currentToken.code == XOR_CODE) {
		op = Operator::XOR;
	}
	else if (currentToken.code == LSL_CODE) {
		op = Operator::LSL;
	}
	else if (currentToken.code == LSR_CODE) {
		op = Operator::LSR;
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
	else if (currentToken.code == AND_CODE) {
		op = Operator::AND;
	}
	else if (currentToken.code == OR_CODE) {
		op = Operator::OR;
	}
	else {
		addError("Expected add operator");
	}

	collectNextToken();
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

bool Compiler::isPostFix() {
	if (currentToken.code == OPEN_BRACE_CODE) {
		return true;
	}

	return false;
}

void Compiler::parsePostFixOp(Operator& op) {
	if (currentToken.code == OPEN_BRACE_CODE) {
		op = Operator::ARRAY_INDEX;
	}
}
