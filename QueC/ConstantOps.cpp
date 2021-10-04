#include "Compiler.h"

// Dedicated to static/const casts
void STATIC_Int8ToInt8(ConstantValue& source) {
}
void STATIC_Int8ToInt16(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	int16_t result = (int16_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int16_t*)&source.value)[0] = result;

	source.type.baseSize = 2;
	source.type.typeCode = TypeCode::TYPE_INT16_CODE;
}
void STATIC_Int8ToInt32(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	int32_t result = (int32_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int32_t*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_INT32_CODE;
}
void STATIC_Int8ToBool(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	int8_t result = value != 0;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;
	source.type.typeCode = TypeCode::TYPE_BOOL_CODE;
}
void STATIC_Int8ToFloat(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	float result = (float)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((float*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_FLOAT32_CODE;
}

void STATIC_Int16ToInt8(ConstantValue& source) {
	int16_t value = ((int16_t*)&source.value)[0];
	int8_t result = (int8_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;

	source.type.baseSize = 1;
	source.type.typeCode = TypeCode::TYPE_INT8_CODE;
}
void STATIC_Int16ToInt16(ConstantValue& source) {
}
void STATIC_Int16ToInt32(ConstantValue& source) {
	int16_t value = ((int16_t*)&source.value)[0];
	int32_t result = (int32_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int32_t*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_INT32_CODE;
}
void STATIC_Int16ToBool(ConstantValue& source) {
	int16_t value = ((int16_t*)&source.value)[0];
	int8_t result = value != 0;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;

	source.type.baseSize = 1;
	source.type.typeCode = TypeCode::TYPE_BOOL_CODE;
}
void STATIC_Int16ToFloat(ConstantValue& source) {
	int16_t value = ((int16_t*)&source.value)[0];
	float result = (float)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((float*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_FLOAT32_CODE;
}

void STATIC_Int32ToInt8(ConstantValue& source) {
	int32_t value = ((int32_t*)&source.value)[0];
	int8_t result = (int8_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;

	source.type.baseSize = 1;
	source.type.typeCode = TypeCode::TYPE_INT8_CODE;
}
void STATIC_Int32ToInt16(ConstantValue& source) {
	int32_t value = ((int32_t*)&source.value)[0];
	int16_t result = (int16_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int16_t*)&source.value)[0] = result;

	source.type.baseSize = 2;
	source.type.typeCode = TypeCode::TYPE_INT16_CODE;
}
void STATIC_Int32ToInt32(ConstantValue& source) {
}
void STATIC_Int32ToBool(ConstantValue& source) {
	int32_t value = ((int32_t*)&source.value)[0];
	int8_t result = value != 0;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;

	source.type.baseSize = 1;
	source.type.typeCode = TypeCode::TYPE_BOOL_CODE;
}
void STATIC_Int32ToFloat(ConstantValue& source) {
	int32_t value = ((int32_t*)&source.value)[0];
	float result = (float)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((float*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_FLOAT32_CODE;
}

void STATIC_BoolToInt8(ConstantValue& source) {
	source.type.typeCode = TypeCode::TYPE_INT8_CODE;
}
void STATIC_BoolToInt16(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	int16_t result = (int16_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int16_t*)&source.value)[0] = result;

	source.type.baseSize = 2;
	source.type.typeCode = TypeCode::TYPE_INT16_CODE;
}
void STATIC_BoolToInt32(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	int32_t result = value != 0;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int32_t*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_INT32_CODE;
}
void STATIC_BoolToBool(ConstantValue& value) {
}
void STATIC_BoolToFloat(ConstantValue& source) {
	int8_t value = ((int8_t*)&source.value)[0];
	float result = (float)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((float*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_FLOAT32_CODE;
}

void STATIC_FloatToInt8(ConstantValue& source) {
	float value = ((float*)&source.value)[0];
	int8_t result = (int8_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;

	source.type.baseSize = 1;
	source.type.typeCode = TypeCode::TYPE_INT8_CODE;
}
void STATIC_FloatToInt16(ConstantValue& source) {
	float value = ((float*)&source.value)[0];
	int16_t result = (int16_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int16_t*)&source.value)[0] = result;

	source.type.baseSize = 2;
	source.type.typeCode = TypeCode::TYPE_INT16_CODE;
}
void STATIC_FloatToInt32(ConstantValue& source) {
	float value = ((float*)&source.value)[0];
	int32_t result = (int32_t)value;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int32_t*)&source.value)[0] = result;

	source.type.baseSize = 4;
	source.type.typeCode = TypeCode::TYPE_INT32_CODE;
}
void STATIC_FloatToBool(ConstantValue& source) {
	float value = ((float*)&source.value)[0];
	int8_t result = value != 0;
	memset(source.value, 0, MAX_CONSTANT_VALUE_SIZE);
	((int8_t*)&source.value)[0] = result;

	source.type.baseSize = 1;
	source.type.typeCode = TypeCode::TYPE_BOOL_CODE;
}
void STATIC_FloatToFloat(ConstantValue& value) {
}

// Includes a cast from each type to each type.
void (*staticCastFunctions[(int)TypeCode::CONSTANT_TYPE_COUNT][(int)TypeCode::CONSTANT_TYPE_COUNT])(ConstantValue& value) {
	{ STATIC_Int8ToInt8, STATIC_Int8ToInt16, STATIC_Int8ToInt32, STATIC_Int8ToBool, STATIC_Int8ToFloat },
	{ STATIC_Int16ToInt8, STATIC_Int16ToInt16, STATIC_Int16ToInt32, STATIC_Int16ToBool, STATIC_Int16ToFloat },
	{ STATIC_Int32ToInt8, STATIC_Int32ToInt16, STATIC_Int32ToInt32, STATIC_Int32ToBool, STATIC_Int32ToFloat },
	{ STATIC_BoolToInt8, STATIC_BoolToInt16, STATIC_BoolToInt32, STATIC_BoolToBool, STATIC_BoolToFloat },
	{ STATIC_FloatToInt8, STATIC_FloatToInt16, STATIC_FloatToInt32, STATIC_FloatToBool, STATIC_FloatToFloat },
};

void Compiler::staticCast(TypeCode startingType, TypeCode endingType, ConstantValue& value) {
	if (startingType >= TypeCode::CONSTANT_TYPE_COUNT || startingType < (TypeCode)0
		|| endingType >= TypeCode::CONSTANT_TYPE_COUNT || endingType < (TypeCode)0) 
	{
		// Illegal operation, this error should always be triggered by 
		// another error which has already been logged.
		return;
	}

	if (pass > 1) {
		if (value.type.ptrCount > 0) {
			staticCastFunctions[(int)startingType][(int)TypeCode::TYPE_INT32_CODE];
		}
		else {
			staticCastFunctions[(int)startingType][(int)endingType](value);
		}
	}
}

void Compiler::writeConstantValueToDataSegment(ConstantValue& value, SymbolInfo& symbolInfo) {
	if (pass > 1 && !errorsFound) {
		staticCast(value.type.typeCode, symbolInfo.typeInfo.typeCode, value);
		std::memcpy(output + symbolInfo.value, value.value, value.type.baseSize);
		symbolInfo.defined = true;
	}
}

void Compiler::getConstantFromDataSegment(ConstantValue& dest, SymbolInfo& info) {
	if (pass > 1 && !errorsFound) {
		std::memcpy(dest.value, output + info.value, info.typeInfo.baseSize);
	}
}

void Compiler::parseConstantArrayInitializer(SymbolInfo& symbolInfo, int previousOffset, int currentDim) {
	if (currentDim >= (int)symbolInfo.dimensions.size()) {
		addError("Too many initializer values for array");
		return;
	}

	int dimIndex = 0;
	int maxDimSize = symbolInfo.dimensions[currentDim];
	int currentByteOffset = 0;
	int startingOffset = symbolInfo.value;

	while (!isEndOfStream()) {
		if (dimIndex >= maxDimSize) {
			addError("Too many initializer values for the array");
			return;
		}

		currentByteOffset = ((symbolInfo.totalDimensionSizes[currentDim] /
			symbolInfo.dimensions[currentDim]) * dimIndex);
		currentByteOffset *= symbolInfo.typeInfo.sizeInMemory();
		currentByteOffset += previousOffset;

		if (currentDim == symbolInfo.dimensions.size() - 1) {
			dimIndex++;
			ConstantValue value;
			symbolInfo.value += currentByteOffset;
			parseConstantExpression(value, symbolInfo, true);
			writeConstantValueToDataSegment(value, symbolInfo);
			symbolInfo.value = startingOffset;
		}
		else if (currentToken.code == OPEN_BRK_CODE) {
			dimIndex++;
			collectNextToken();
			parseConstantArrayInitializer(symbolInfo, currentByteOffset, currentDim + 1);

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
}

/// <summary>
/// Parse like this:
/// expr = [-] (expr) | term +/- expr
/// term = (term) | term *// expr
/// We know its constant, so we can simplify each term as we go. Operator overloading
/// will not be constant if I ever implement that.
/// </summary>
void Compiler::parseConstantExpression(ConstantValue& left, SymbolInfo& targetSymbol, bool allowRefs) {
	ConstantValue right;
	Operator op;
	int negate = 1;
        
	// check optional sign  
	if (currentToken.code == MINUS_CODE) {
		collectNextToken();
		//eat the sign, will negate in code generation
		negate = -1;
	}

	parseConstantTerm(left, targetSymbol, allowRefs);
        
	//stop being so negative, this isn't that inefficient!
	//multiply by -1 if the negation is there
	if(negate == -1) {
		//int minusOneIndex = symbolList.AddSymbol("-1", 'c', -1);
		ConstantValue negateConstant;
		negateConstant.type.baseSize = left.type.baseSize;
		negateConstant.type.typeCode = left.type.typeCode;
		negateConstant.type.ptrCount = 0;
		((int32_t*)&negateConstant.value)[0] = -1;
		performOperatorOnConstants(left, negateConstant, left, Operator::MULT);
	}

	while (isAddop() && (!isEndOfStream()) && (!errorsFound)) {
		parseAddOp(op);
		parseConstantTerm(right, targetSymbol, allowRefs);
		performOperatorOnConstants(left, right, left, op);
	}
}

void Compiler::parseConstantTerm(ConstantValue& left, SymbolInfo& targetSymbol, bool allowRefs) {
	ConstantValue right;
	Operator op;
        
	parseConstantFactor(left, targetSymbol, allowRefs);

	while ((isMulop() && (!isEndOfStream()) && (!errorsFound))) {
		parseMulOp(op);
		parseConstantFactor(right, targetSymbol, allowRefs);
		performOperatorOnConstants(left, right, left, op);
	}
}

//<factor> -> <unsigned constant> | <variable> | $LPAR <simple expression> $RPAR
void Compiler::parseConstantFactor(ConstantValue& value, SymbolInfo& targetSymbol, bool allowRefs) {
	// unsigned constant starts with integer or float number
	if (getConstantValueFromToken(value, targetSymbol, allowRefs)) {
	}
	else if (currentToken.code == OPEN_PARN_CODE) {
		collectNextToken();
		parseConstantExpression(value, targetSymbol, allowRefs);

		if (currentToken.code == CLOSE_PARN_CODE) {
			collectNextToken();
		}
		else {
			addError("Expected ')'");
		}
	}
	else if (currentToken.code == MINUS_CODE) {
		parseConstantExpression(value, targetSymbol, allowRefs);
	}
	else {
		addError("Expected constant value");
	}
}

bool Compiler::getConstantValueFromToken(ConstantValue& value, SymbolInfo& targetSymbol, bool allowRefs) {
	if (currentToken.code == IDENTIFIER_CODE) {
		return getConstantValueFromIdentifier(value, targetSymbol, allowRefs);
	}
	else if (currentToken.code == FLOAT_CODE) {
		value.type.baseSize = types.getTypeSize(TypeCode::TYPE_FLOAT32_CODE);
		value.type.ptrCount = 0;
		value.type.typeCode = TypeCode::TYPE_FLOAT32_CODE;

		float loadedValue = tokenTo32BitFloat();
		((float*)&value.value)[0] = loadedValue;
		collectNextToken();
	}
	else if (currentToken.code == INTEGER_CODE || currentToken.code == HEX_INTEGER_CODE ||
		currentToken.code == OCTAL_INTER_CODE || currentToken.code == BINARY_INTEGER_CODE) {
		value.type.baseSize = types.getTypeSize(TypeCode::TYPE_INT32_CODE);
		value.type.ptrCount = 0;
		value.type.typeCode = TypeCode::TYPE_INT32_CODE;

		int loadedValue = tokenTo32BitInt();
		((int*)&value.value)[0] = loadedValue;
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("true")) {
		value.type.baseSize = 1;
		value.type.ptrCount = 0;
		value.type.typeCode = TypeCode::TYPE_BOOL_CODE;

		int loadedValue = 1;
		((int*)&value.value)[0] = loadedValue;
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("false")) {
		value.type.baseSize = 1;
		value.type.ptrCount = 0;
		value.type.typeCode = TypeCode::TYPE_BOOL_CODE;

		int loadedValue = 0;
		((int*)&value.value)[0] = loadedValue;
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("ref")) {
		// A reference can only contain a symbol, nothing else, ever.
		// It simply returns the address of that identifier in memory.
		// We already know this variable has to be in the data segment, so we don't have
		// to figure out where it is on stack.
		collectNextToken();
		parseConstantRef(value, targetSymbol, allowRefs);
	}
	else if (currentToken.code == reserveTable.getReserveCode("deref")) {
		addError("Dereferences cannot be resolved to a constant value");
		collectNextToken();
		return false;
	}
	else {
		return false;
	}

	return true;
}

bool Compiler::parseConstantRef(ConstantValue& value, SymbolInfo& targetSymbol, bool allowRefs) {
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

	if (pass == 0) {
		return success;
	}

	// Now we can figure out what the address is
	if (success) {
		getConstantAddressOfSymbol(identifierName, value, targetSymbol, allowRefs);
	}

	return errorsFound;
}

void Compiler::getConstantAddressOfSymbol(const std::string& symbolName, ConstantValue& value, SymbolInfo& targetSymbol, bool allowRefs) {
	if (!allowRefs || targetSymbol.accessType == QueSymbolAccessType::COMPILER_ACCESS) {
		addError("Cannot assign reference to constant");
	}

	SymbolInfo* symbolInfo = symbolStack.searchSymbol(symbolName);

	if (symbolInfo) {
		if (symbolInfo->accessType != QueSymbolAccessType::IMPORTED) {
			if (symbolInfo->symbolType == QueSymbolType::DATA) {
				value.type.baseSize = symbolInfo->typeInfo.baseSize;
				value.type.typeCode = symbolInfo->typeInfo.typeCode;
			}
			else if (symbolInfo->symbolType == QueSymbolType::FUNCTION ||
				symbolInfo->symbolType == QueSymbolType::ARRAY) {
				value.type.baseSize = 4;
				value.type.typeCode = TypeCode::TYPE_INT32_CODE;
			}
			else {
				// TODO: deal with imported
				addError("Attempting to obtain a reference for an invalid symbol: '" + symbolInfo->name + "'");
			}

			((uint32_t*)&value.value)[0] = symbolInfo->value;
			value.type.ptrCount = symbolInfo->typeInfo.ptrCount + 1;

			// Add usage of local symbol to the global offset table
			if (pass > 1) {
				symbolStack.globalSymbols.addLocalDataSegmentReference(targetSymbol.value);
			}
		}
		else {
			addError("Cannot get a constant reference for imported symbols");
		}
	}
	else {
		addError("Undeclared identifier: " + symbolName);
	}
}

bool Compiler::getConstantValueFromIdentifier(ConstantValue& value, SymbolInfo& targetSymbol, bool allowRefs) {
	bool allowLbls = targetSymbol.symbolType != QueSymbolType::CONST_EXPR;
	SymbolInfo* info = symbolStack.searchSymbol(currentToken.lexeme);
	std::string name;

	if (info) {
		name = currentToken.lexeme;
		value.type.baseSize = info->typeInfo.baseSize;
		value.type.ptrCount = info->typeInfo.ptrCount;
		value.type.typeCode = info->typeInfo.typeCode;
	}
	else {
		// If the symbol isnt defined, we only care if we are on the second pass, after all symbols have been defined.
		if (pass != 0) {
			addError("Undefined symbol");
		}
		collectNextToken();
		return pass == 0;
	}

	//if (!info.defined && pass != 0) {
		//addError("Attempting to access variable before it's initialized");
	//}

	// If we are allowing refs, it has to be a value that's resolvable at runtime like the address
	// of operator.
	// If we are here, the only types which hold addresses are arrays and functions.

	if (info->accessType != QueSymbolAccessType::COMPILER_ACCESS) {
		if (allowRefs) {
			if (info->symbolType == QueSymbolType::FUNCTION || info->symbolType == QueSymbolType::ARRAY) {
				getConstantAddressOfSymbol(name, value, targetSymbol, allowRefs);
			}
			else if (info->symbolType == QueSymbolType::DATA) {
				addError("Variable does not have a constant value.");
				//getConstantFromDataSegment(value, info);
			}
		}
		else {
			addError("References not allowed in this context");
		}
	}
	else {
		((int*)&value.value)[0] = info->value;
	}

	collectNextToken();
	return true;
}

void Compiler::performOperatorOnConstants(ConstantValue& left, ConstantValue& right, ConstantValue& dest, Operator operatorCode) {
	// Figure out what type the result is.
	// It should be expanded to the widest type, and if one's a float
	// The result should be a float.

	// Cast both values to widest type, float is considered higher priority than integer.
	TypeCode highestPriorityCode = TypeCode::TYPE_INT32_CODE;

	if (left.type.typeCode == TypeCode::TYPE_FLOAT32_CODE || right.type.typeCode == TypeCode::TYPE_FLOAT32_CODE) {
		highestPriorityCode = TypeCode::TYPE_FLOAT32_CODE;
	}
	else {
		highestPriorityCode = TypeCode::TYPE_INT32_CODE;
	}

	staticCast(left.type.typeCode, highestPriorityCode, left);
	staticCast(right.type.typeCode, highestPriorityCode, right);

	dest.type.baseSize = left.type.baseSize;
	dest.type.ptrCount = 0;
	dest.type.typeCode = left.type.typeCode;

	// Now that they're both the same types. Lets perform the operator and store the result in dest
	if (highestPriorityCode == TypeCode::TYPE_FLOAT32_CODE) {
		float leftValue = ((float*)&left.value)[0];
		float rightValue = ((float*)&right.value)[0];
		float result = 0;
		
		if (operatorCode == Operator::ADD) {
			result = leftValue + rightValue;
		}
		else if (operatorCode == Operator::SUB) {
			result = leftValue - rightValue;
		}
		else if (operatorCode == Operator::MULT) {
			result = leftValue * rightValue;
		}
		else if (operatorCode == Operator::DIV) {
			result = leftValue / rightValue;
		}
		else if (operatorCode == Operator::MOD) {
			addError("Modlus is invalid for floating points");
		}

		((float*)dest.value)[0] = result;
	}
	else {
		int leftValue = ((int*)&left.value)[0];
		int rightValue = ((int*)&right.value)[0];
		int result = 0;

		if (operatorCode == Operator::ADD) {
			result = leftValue + rightValue;
		}
		else if (operatorCode == Operator::SUB) {
			result = leftValue - rightValue;
		}
		else if (operatorCode == Operator::MULT) {
			result = leftValue * rightValue;
		}
		else if (operatorCode == Operator::DIV) {
			result = leftValue / rightValue;
		}
		else if (operatorCode == Operator::MOD) {
			result = leftValue % rightValue;
		}

		((int*)dest.value)[0] = result;
	}
}

