#include "Compiler.h"

#define TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE -1

void Compiler::parseExpression(SymbolInfo*& left) {
	Operator op;
	bool hasPrefixOp = false;
	Operator prefixOp;

	// Each expression can get ONE prefix operator.
	if (isPrefixOp()) {
		parsePrefixOp(prefixOp);
		hasPrefixOp = true;
	}

	parseCompareExpr(left);

	// Compute the prefix operator
	if (hasPrefixOp) {
		writePrefixOperatorInstructions(*left, prefixOp);
	}
        
	while ((isLogicalOp()) && (!isEndOfStream()) && (!errorsFound)) {
		// Create temporary variable for ad op.
		SymbolInfo* right = new SymbolInfo(symbolStack.scopeIndex);

		if (isLogicalOp()) {
			parseLogicalOp(op);
			writeLogicalOperatorInstructions(*left, right, op);
		}

		// Don't need to hold on to temporary registers
		symbolStack.freeIntRegister(*right, false);
	}
}

void Compiler::parseCompareExpr(SymbolInfo*& left) {
	Operator op;
        
	parseStandardExpr(left);

	while ((isCompareOp()) && (!isEndOfStream()) && (!errorsFound)) {
		SymbolInfo* right = new SymbolInfo(symbolStack.scopeIndex);

		if (isCompareOp()) {
			parseCompareOp(op);
			parseStandardExpr(right);
			writeCompareOperatorInstructions(*left, *right, op);
		}

		// Don't need to hold on to temporary registers
		symbolStack.freeIntRegister(*right, false);
	}
}

void Compiler::parseStandardExpr(SymbolInfo*& left) {
	Operator op;
        
	parseTerm(left);

	while ((isAddop()) && (!isEndOfStream()) && (!errorsFound)) {
		SymbolInfo* right = new SymbolInfo(symbolStack.scopeIndex);

		if (isAddop()) {
			parseAddOp(op);
			parseTerm(right);
			writeOperatorInstructions(*left, *right, op);
		}

		// Don't need to hold on to temporary registers
		symbolStack.freeIntRegister(*right, false);
	}
}

void Compiler::parseTerm(SymbolInfo*& left) {
	Operator op;
        
	parsePostFix(left);

	while ((isMulop()) && (!isEndOfStream()) && (!errorsFound)) {
		SymbolInfo* right = new SymbolInfo(symbolStack.scopeIndex);

		if (isMulop()) {
			parseMulOp(op);
			parsePostFix(right);
			writeOperatorInstructions(*left, *right, op);
		}

		// Don't need to hold on to temporary registers
		symbolStack.freeIntRegister(*right, false);
	}
}

void Compiler::parsePostFix(SymbolInfo*& left) {
	Operator op;
        
	parseFactor(left);

	while ((isPostFix() && (!isEndOfStream()) && (!errorsFound))) {
		parsePostFixOp(op);
		if (op == Operator::ARRAY_INDEX) {
			collectNextToken();

			if (left->symbolType == QueSymbolType::ARRAY) {
				parseArrayIndexer(left);
			}
			else if (left->symbolType == QueSymbolType::DATA && left->typeInfo.ptrCount > 0) {
				SymbolInfo* right = new SymbolInfo(symbolStack.scopeIndex);
				parseExpression(right);
				writePtrIndexInstructions(*left, *right);
				symbolStack.freeIntRegister(*right, false);

				if (currentToken.code == CLOSE_BRACE_CODE) {
					collectNextToken();
				}
				else {
					addError("Expected ']'");
				}
			}
			else {
				addError("Array index operator invalid on this type");
			}
		}
	}
}

//<factor> -> <unsigned constant> | <variable> | $LPAR <simple expression> $RPAR
void Compiler::parseFactor(SymbolInfo*& targetSymbol) {
	// unsigned constant starts with integer or float number
	if (getValueFromToken(targetSymbol)) {
	}
	else if (currentToken.code == OPEN_PARN_CODE) {
		collectNextToken();
		parseExpression(targetSymbol);

		if (currentToken.code == CLOSE_PARN_CODE) {
			collectNextToken();
		}
		else {
			addError("Expected ')'");
		}
	}
	else if (isPrefixOp()) {
		parseExpression(targetSymbol);
	}
	else {
		addError("Expected a factor");
	}

	if (targetSymbol->symbolType == QueSymbolType::FUNCTION) {
		if (currentToken.code == OPEN_PARN_CODE) {
			collectNextToken();
			parseFunctionCall(targetSymbol, targetSymbol);
		}
	}
}

void Compiler::parseIdentifierValue(SymbolInfo*& symbolInfo) {
	SymbolInfo* info = symbolStack.searchSymbol(currentToken.lexeme);
	std::string name = symbolInfo->name;

	if (info) {
		*symbolInfo = *info;
		symbolInfo->name = name;

		if (symbolInfo->accessType == QueSymbolAccessType::COMPILER_ACCESS) {
			symbolInfo->isTemporaryValue = true;
		}
		else {
			if (symbolInfo->symbolType == QueSymbolType::ARRAY) {
				symbolInfo->typeInfo.ptrCount++;
			}
			else if (symbolInfo->symbolType == QueSymbolType::FUNCTION) {
				// The result now has to be in a register.
			}

			symbolInfo->isTemporaryValue = false;
		}
	}
	else {
		// If the symbol isnt defined, we only care if we are on the second pass, after all symbols have been defined.
		if (pass != 0) {
			addError("Undefined symbol");
		}
	}

	collectNextToken();
}

/// <summary>
/// Returns a variable 
/// </summary>
/// <param name="info"></param>
/// <returns></returns>
bool Compiler::getValueFromToken(SymbolInfo*& info) {
	info->name = "@" + std::to_string(symbolStack.getTemporaryVariableIndex());
	info->typeInfo.baseSize = types.getTypeSize(TypeCode::TYPE_INT32_CODE);
	info->typeInfo.ptrCount = 0;
	info->typeInfo.typeCode = TypeCode::TYPE_INT32_CODE;
	info->isGlobal = false;
	info->isTemporaryValue = true;
	info->value = TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE;
	info->temporaryRegisterBPOffset = TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE;
	symbolStack.addSymbolToCurrentScope(info);

	if (currentToken.code == IDENTIFIER_CODE) {
		parseIdentifierValue(info);
		info->temporaryRegisterBPOffset = TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE;
	}
	else if (currentToken.code == FLOAT_CODE) {
		// TODO
		collectNextToken();
	}
	else if (currentToken.code == INTEGER_CODE || currentToken.code == HEX_INTEGER_CODE ||
		currentToken.code == OCTAL_INTER_CODE || currentToken.code == BINARY_INTEGER_CODE)
	{
		// Create temporary register to store constant value.
		info->isTemporaryValue = true;

		int loadedValue = tokenTo32BitInt();
		info->value = loadedValue;

		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("true")) {
		// Create temporary register to store constant value.
		info->isTemporaryValue = true;

		int loadedValue = 1;
		info->value = loadedValue;

		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("false")) {
		// Create temporary register to store constant value.
		int loadedValue = 0;
		info->value = loadedValue;

		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("ref")) {
		collectNextToken();
		// Create local stack space.
		info->isTemporaryValue = false;
		info->accessType = QueSymbolAccessType::VARIABLE;
		info->symbolType = QueSymbolType::DATA;

		parseRef(*info);
	}
	else if (currentToken.code == reserveTable.getReserveCode("deref")) {
		collectNextToken();
		info->isTemporaryValue = false;
		info->accessType = QueSymbolAccessType::VARIABLE;
		info->symbolType = QueSymbolType::DATA;

		// Deref is nice and simple, it can be an expression which resolves to an integer
		if (currentToken.code == OPEN_PARN_CODE) {
			collectNextToken();
			parseDeref(info);
			if (currentToken.code != CLOSE_PARN_CODE) {
				addError("Expected ')'");
			}
			collectNextToken();
		}
		else {
			addError("Expected '('");
		}
	}
	else if (currentToken.code == reserveTable.getReserveCode("sizeof")) {
		collectNextToken();
		if (currentToken.code == OPEN_PARN_CODE) {
			collectNextToken();
			parseSizeOf(info);
			if (currentToken.code != CLOSE_PARN_CODE) {
				addError("Expected ')'");
			}
			collectNextToken();
		}
		else {
			addError("Expected '('");
		}
	}
	else {
		return false;
	}

	return true;
}

void Compiler::parseSizeOf(SymbolInfo*& symbol) {
	bool success = false;
	std::string identifierName;
	if (currentToken.code == IDENTIFIER_CODE) {
		identifierName = currentToken.lexeme;
		collectNextToken();
		success = true;
	}
	else {
		addError("Expected identifier");
	}

	// Now we can figure out what the address is
	if (success) {
		SymbolInfo* info = symbolStack.searchSymbol(identifierName);
		if (info) {
			// Determine the total size of the variable in bytes and move to the immediate valeu
			symbol->isTemporaryValue = true;
			symbol->value = info->calculateTotalSize();
		}
		else {
			if (pass != 0) {
				addError("Undeclared identifier: " + identifierName);
			}
		}
	}
}

void Compiler::parseDeref(SymbolInfo*& info) {
	// Inside, we just need an expression, and we will save it to a temporary variable.
	parseExpression(info);

	if (info->isTemporaryValue || info->typeInfo.ptrCount < 1) {
		addError("Cannot dereference non-pointer types");
	}
	else {
		info->typeInfo.ptrCount--;
		int addressToStore = R1;
		int reg = symbolStack.allocateIntRegister(*info, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);

		// Read the value into dereference reg
		writeLoadValueFromAddressInstructions(*info, 0, reg, reg);
	}
}

void Compiler::getAddressOfSymbol(const std::string& idtName, SymbolInfo& value) {
	SymbolInfo* symbolInfo = symbolStack.searchSymbol(idtName);

	if (symbolInfo != nullptr) {
		if (symbolInfo->accessType == QueSymbolAccessType::COMPILER_ACCESS) {
			addError("Cannot obtain address for compile-time only variable");
		}

		if (symbolInfo->accessType != QueSymbolAccessType::IMPORTED) {
			if (symbolInfo->symbolType == QueSymbolType::DATA) {
				value.typeInfo.baseSize = symbolInfo->typeInfo.baseSize;
				value.typeInfo.typeCode = symbolInfo->typeInfo.typeCode;
			}
			else if (symbolInfo->symbolType == QueSymbolType::FUNCTION ||
				symbolInfo->symbolType == QueSymbolType::ARRAY) {
				value.typeInfo.baseSize = symbolInfo->typeInfo.baseSize;
				value.typeInfo.typeCode = symbolInfo->typeInfo.typeCode;
			}

			value.typeInfo.ptrCount = symbolInfo->typeInfo.ptrCount + 1;

			// Get the address of the symbol into addressReg
			int addressReg = symbolStack.allocateIntRegister(value, RegisterAllocMode::DONT_LOAD);
			int offsetFromBP = writeLoadAddressInstructions(*symbolInfo, addressReg);
			if (offsetFromBP != 0) {
				writeInstruction(
					encodeInstruction(ADD, true, addressReg, BP), true, offsetFromBP);
			}
		}
		else {
			// TODO: deal with imported symbols
			addError("Cannot get a reference for imported symbols (yet)");
		}
	}
	else {
		if (pass != 0) {
			addError("Undeclared identifier: " + idtName);
		}
	}
}

void Compiler::parseRef(SymbolInfo& info) {
	std::string identifierName;
	bool success = false;

	if (currentToken.code == OPEN_PARN_CODE) {
		collectNextToken();
		if (currentToken.code == IDENTIFIER_CODE) {
			identifierName = currentToken.lexeme;
			collectNextToken();
			if (currentToken.code == CLOSE_PARN_CODE) {
				collectNextToken();
				success = true;
			}
		}
		else {
			addError("Expected identifier");
		}
	}
	else {
		addError("Expected '('");
	}

	// Now we can figure out what the address is
	if (success) {
		getAddressOfSymbol(identifierName, info);
	}
}

/// <summary>
/// The variable better exist before this function is called.
/// </summary>
/// <param name="variable"></param>
int SymbolStack::allocateIntRegister(SymbolInfo& info, RegisterAllocMode mode) {
	// See if we already have a register for that variable.
	// If so, return it.
	std::map<std::string, Register>::iterator registerUsed = registerAllocationStack.find(info.name);

	if (registerUsed != registerAllocationStack.end()) {
		// Move the register to the top
		registerInUse.push_back((Register)registerUsed->second);

		for (int i = 0; i < registerInUse.size(); i++) {
			if (registerInUse[i] == registerUsed->second) {
				registerInUse.erase(registerInUse.begin() + i);
				break;
			}
		}

		return registerUsed->second;
	}

	// We can only work with the register alloc state at the current stack frame,
	// but the variables we have to retrieve can be at lower stack frames.
	// We are free to use registers 2-15 at will.
	int toUse = 0;

	if (registerInUse.size() > (R15 - USABLE_REGISTER_START)) {
		// Remove the register used the longest time ago, store the result on stack.
		toUse = registerInUse[0];

		// Find out which symbol is using it, then free that symbol
		SymbolInfo* toFree = nullptr;
		for (std::map<std::string, Register>::iterator i =
			registerAllocationStack.begin(); i != registerAllocationStack.end();
			i++)
		{
			if (i->second == toUse) {
				toFree = searchSymbol(i->first);
				break;
			}
		}

		if (toFree) {
			freeIntRegister(*toFree, true);
		}
	}
	else {
		toUse = availableRegisters.top();
		availableRegisters.pop();
	}

	registerInUse.push_back((Register)toUse);
	registerAllocationStack[info.name] = (Register)toUse;

	// Once we get a register, if we require a register load, load the value in.
	if (mode == RegisterAllocMode::LOAD_ADDRESS_OR_VALUE) {
		compiler->writeLoadValueOrAddressInstructions(info, toUse);
	}
	else if (mode == RegisterAllocMode::LOAD_VALUE) {
		compiler->writeLoadValueInstructions(info, toUse);
	}

	return toUse;
}

/// <summary>
/// Should be called after the value of a variable is safely stored in it's location.
/// </summary>
/// <param name="variable"></param>
void SymbolStack::freeIntRegister(SymbolInfo& info, bool preserveValue) {
	// If the variable is a temporary value, write it to memory and change it to a local
	std::map<std::string, Register>::iterator registerUsed =
		registerAllocationStack.find(info.name);

	if (registerUsed != registerAllocationStack.end()) {
		int registerToFree = registerUsed->second;
		registerAllocationStack.erase(registerUsed);

		for (int i = 0; i < registerInUse.size(); i++) {
			if (registerInUse[i] == (Register)registerToFree) {
				registerInUse.erase(registerInUse.begin() + i);
				break;
			}
		}

		if (preserveValue) {
			// Put the variable in its proper location on stack if needed
			if (info.isTemporaryValue || info.value == TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE ||
				info.temporaryRegisterBPOffset == TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE) {
				// Create local stack space.
				info.isTemporaryValue = false;
				info.accessType = QueSymbolAccessType::VARIABLE;
				info.symbolType = QueSymbolType::DATA;
				info.isGlobal = false;
				pushVariable(info);
				info.temporaryRegisterBPOffset = info.value;
			}

			// Store the result on stack.
			int offset = compiler->symbolStack.calculateBPOffsetFromCurrentStackFrame(info);
			compiler->writeStoreValueInAddressInstructions(info, offset, BP, registerToFree);
		}

		availableRegisters.push((Register)registerToFree);
	}
	else {
		if (preserveValue) {
			std::cout << "Attempted to free an unallocated variable (This is an internal compiler bug.";
		}
	}
}

void Compiler::parseArrayInitializer(SymbolInfo*& symbolInfo, SymbolInfo*& variableSymbol, int previousOffset, int currentDim, bool fillZeros) {
	if (currentDim >= (int)symbolInfo->dimensions.size()) {
		addError("Too many initializer values for array");
		return;
	}

	int initialBPOffset = symbolStack.calculateBPOffsetFromCurrentStackFrame(*symbolInfo);
	int dimIndex = 0;
	int maxDimSize = symbolInfo->dimensions[currentDim];
	int currentByteOffset = 0;

	while (!isEndOfStream() && !fillZeros) {
		if (dimIndex >= maxDimSize) {
			addError("Too many initializer values for the array");
			return;
		}

		currentByteOffset = ((symbolInfo->totalDimensionSizes[currentDim] /
			symbolInfo->dimensions[currentDim]) * dimIndex);
		currentByteOffset *= symbolInfo->typeInfo.sizeInMemory();
		currentByteOffset += previousOffset;

		if (currentDim == symbolInfo->dimensions.size() - 1) {
			dimIndex++;
			parseExpression(variableSymbol);
			int reg = symbolStack.allocateIntRegister(*variableSymbol, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);

			// Parse the expression and write it to the array as a BP offset.
			writeStoreValueInAddressInstructions(*variableSymbol, currentByteOffset + initialBPOffset, BP, reg);
			symbolStack.freeIntRegister(*variableSymbol, false);
		}
		else if (currentToken.code == OPEN_BRK_CODE) {
			dimIndex++;
			collectNextToken();
			parseArrayInitializer(symbolInfo, variableSymbol, currentByteOffset, currentDim + 1, false);

			if (currentToken.code != CLOSE_BRK_CODE) {
				addError("Expected '}'");
			}

			collectNextToken();
		}
		else {
			addError("Expected '{'");
			break;
		}

		if (currentToken.code != COMMA_CODE) {
			break;
		}

		collectNextToken();
	}

	// Fill the rest with zeros
	// R1 is reserved, and as long as we are not allocating new registers, it's completely fine to use
	int reg = R1;
	if (currentDim == symbolInfo->dimensions.size() - 1) {
		writeInstruction(encodeInstruction(MOV, true, reg), true, 0);
	}

	for (int i = dimIndex; i < maxDimSize; i++) {
		currentByteOffset = ((symbolInfo->totalDimensionSizes[currentDim] /
			symbolInfo->dimensions[currentDim]) * i);
		currentByteOffset *= symbolInfo->typeInfo.sizeInMemory();
		currentByteOffset += previousOffset;

		if (currentDim == symbolInfo->dimensions.size() - 1) {
			// Parse the expression and write it to the array as a BP offset.
			writeStoreValueInAddressInstructions(*variableSymbol, currentByteOffset + initialBPOffset, BP, reg);
		}
		else {
			parseArrayInitializer(symbolInfo, variableSymbol, currentByteOffset, currentDim + 1, true);
		}
	}
}
