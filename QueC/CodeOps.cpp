#include "Compiler.h"

#define TEMPORARY_VARIABLE_NOT_ON_STACK_VALUE 0xFFFFFFFF

void Compiler::parseExpression(SymbolInfo*& left) {
	Operator op;

	int negate = 1;

	// check optional sign  
	if (currentToken.code == MINUS_CODE) {
		collectNextToken();
		negate = -1;
	}

	parseTerm(left);

	//stop being so negative, this isn't that inefficient!
	//multiply by -1 if the negation is there
	if(negate == -1) {
		//int minusOneIndex = symbolList.AddSymbol("-1", 'c', -1);
		//((int32_t*)&negateConstant.value)[0] = -1;
		//performOperatorOnConstants(left, negateConstant, left, Operator::MULT);
		// Negate left
	}
        
	while ((isAddop() || isCompareOp()) && (!isEndOfStream()) && (!errorsFound)) {
		// Create temporary variable for ad op.
		SymbolInfo* right = new SymbolInfo();

		if (isAddop()) {
			parseAddOp(op);
			parseTerm(right);
			writeOperatorInstructions(*left, *right, op);
		}
		else if(isCompareOp()) {
			parseCompareOp(op);
			parseTerm(right);
			writeCompareOperatorInstructions(*left, *right, op);
		}

		// Don't need to hold on to temporary registers
		symbolStack.freeIntRegister(*right, false);
	}
}

void Compiler::parseTerm(SymbolInfo*& left) {
	Operator op;
        
	parseFactor(left);

	while ((isMulop() && (!isEndOfStream()) && (!errorsFound))) {
		parseMulOp(op);
		SymbolInfo* right = new SymbolInfo();
		parseFactor(right);
		writeOperatorInstructions(*left, *right, op);

		// Don't need to hold on to temporary registers
		symbolStack.freeIntRegister(*right, false);
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
	else if (currentToken.code == MINUS_CODE) {
		parseExpression(targetSymbol);
	}
	else {
		addError("Expected a value");
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
			if (symbolInfo->symbolType == QueSymbolType::ARRAY || symbolInfo->symbolType == QueSymbolType::FUNCTION) {
				symbolInfo->typeInfo.ptrCount++;
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
	std::map<std::string, Register>::iterator registerUsed =
		registerAllocationStack[scopeIndex].find(info.name);

	if (registerUsed != registerAllocationStack[scopeIndex].end()) {
		// Move the register to the top
		registerInUse[scopeIndex].push_back((Register)registerUsed->second);

		for (int i = 0; i < registerInUse[scopeIndex].size(); i++) {
			if (registerInUse[scopeIndex][i] == registerUsed->second) {
				registerInUse[scopeIndex].erase(registerInUse[scopeIndex].begin() + i);
				break;
			}
		}

		return registerUsed->second;
	}

	// We can only work with the register alloc state at the current stack frame,
	// but the variables we have to retrieve can be at lower stack frames.
	// We are free to use registers 2-15 at will.
	int toUse = 0;

	if (registerInUse[scopeIndex].size() > (R15 - USABLE_REGISTER_START)) {
		// Remove the register used the longest time ago, store the result on stack.
		toUse = registerInUse[scopeIndex][0];

		// Find out which symbol is using it, then free that symbol
		SymbolInfo* toFree = nullptr;
		for (std::map<std::string, Register>::iterator i =
			registerAllocationStack[scopeIndex].begin(); i != registerAllocationStack[scopeIndex].end();
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
		toUse = availableRegisters[scopeIndex].top();
		availableRegisters[scopeIndex].pop();
	}

	registerInUse[scopeIndex].push_back((Register)toUse);
	registerAllocationStack[scopeIndex][info.name] = (Register)toUse;

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
		registerAllocationStack[scopeIndex].find(info.name);

	if (registerUsed != registerAllocationStack[scopeIndex].end()) {
		int registerToFree = registerUsed->second;
		registerAllocationStack[scopeIndex].erase(registerUsed);

		for (int i = 0; i < registerInUse[scopeIndex].size(); i++) {
			if (registerInUse[scopeIndex][i] == (Register)registerToFree) {
				registerInUse[scopeIndex].erase(registerInUse[scopeIndex].begin() + i);
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

		availableRegisters[scopeIndex].push(registerToFree);
	}
	else {
		std::cout << "Something went very wrong freeing register\n";
	}
}
