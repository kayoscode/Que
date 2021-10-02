#include "Compiler.h"

void Compiler::parseExpression(SymbolInfo& targetSymbol) {
	Operator op;
	int negate = 1;
        
	// check optional sign  
	if (currentToken.code == MINUS_CODE) {
		collectNextToken();
		//eat the sign, will negate in code generation
		negate = -1;
	}

	//parseTerm(targetSymbol);
        
	//stop being so negative, this isn't that inefficient!
	//multiply by -1 if the negation is there
	if(negate == -1) {
		//int minusOneIndex = symbolList.AddSymbol("-1", 'c', -1);
		//((int32_t*)&negateConstant.value)[0] = -1;
		//performOperatorOnConstants(left, negateConstant, left, Operator::MULT);
		// Negate left
	}

	while (isAddop() && (!isEndOfStream()) && (!errorsFound)) {
		parseAddOp(op);
		parseExpression(targetSymbol);
		//performOperator(left, right, left, op);
	}
}

void Compiler::parseTerm(SymbolInfo& targetSymbol) {
	ConstantValue right;
	Operator op;
        
	//parseFactor(targetSymbol);

	while ((isMulop() && (!isEndOfStream()) && (!errorsFound))) {
		parseMulOp(op);
		parseExpression(targetSymbol);
		//performOperator()
	}
}

//<factor> -> <unsigned constant> | <variable> | $LPAR <simple expression> $RPAR
void Compiler::parseFactor(SymbolInfo& targetSymbol) {
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
	else {
		addError("Expected a value");
	}
}

/// <summary>
/// Returns a variable 
/// </summary>
/// <param name="info"></param>
/// <returns></returns>
bool Compiler::getValueFromToken(SymbolInfo& info) {
	if (currentToken.code == IDENTIFIER_CODE) {
		collectNextToken();
	}
	else if (currentToken.code == FLOAT_CODE) {
		collectNextToken();
	}
	else if (currentToken.code == INTEGER_CODE || currentToken.code == HEX_INTEGER_CODE ||
		currentToken.code == OCTAL_INTER_CODE || currentToken.code == BINARY_INTEGER_CODE) 
	{
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("true")) {
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("false")) {
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("ref")) {
		collectNextToken();
	}
	else if (currentToken.code == reserveTable.getReserveCode("deref")) {
		collectNextToken();
	}
	else {
		return false;
	}

	return true;
}
