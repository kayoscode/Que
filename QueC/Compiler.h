#pragma once

#include <string>
#include <map>
#include <vector>
#include <cassert>
#include <stack>

#include "../QueASM/Assembler.h"
#include "Symbols.h"
#include "Types.h"

/// <summary>
/// Given an instruction and raw parameters, calculate an opcode.
/// This instruction can create an inexecutable opcode if bad parameters are given.
/// </summary>
/// <param name="opcode"></param>
/// <param name="immediateFlag"></param>
/// <param name="op1"></param>
/// <param name="op2"></param>
/// <param name="op3"></param>
/// <returns></returns>
extern uint32_t encodeInstruction(uint8_t opcode, bool immediateFlag, uint8_t op1 = 0, uint8_t op2 = 0, uint8_t op3 = 0);

constexpr int COMMA_CODE = 108;
constexpr int COLON_CODE = 109;
constexpr int SEMICOLON_CODE = 110;
constexpr int STAR_CODE = 111;
constexpr int OPEN_PARN_CODE = 112;
constexpr int CLOSE_PARN_CODE = 113;
constexpr int OPEN_BRK_CODE = 114;
constexpr int CLOSE_BRK_CODE = 115;
constexpr int EQUALS_CODE = 116;
constexpr int MINUS_CODE = 117;
constexpr int PLUS_CODE = 118;
constexpr int DIV_CODE = 119;
constexpr int OPEN_BRACE_CODE = 120;
constexpr int CLOSE_BRACE_CODE = 121;
constexpr int MODULUS_CODE = 122;

// Compare ops
constexpr int GT_CODE = 123;
constexpr int LT_CODE = 124;
constexpr int GTE_CODE = 125;
constexpr int LTE_CODE = 126;
constexpr int ARE_EQUAL_CODE = 127;
constexpr int NE_CODE = 128;

#define MAX_CONSTANT_VALUE_SIZE 8

/// <summary>
/// Holds data for a constant value.
/// </summary>
struct ConstantValue {
	// Reserved 8 chars, even though we will only ever need 4
	char value[MAX_CONSTANT_VALUE_SIZE] = { 0 };
	TypeInfo type;
};

/// <summary>
/// Class responsible for producing an object file from source code.
/// Should produce the same style of object as the assembler. I want the 
/// two langauges to be directly compatible such that a qo file generated
/// by the compiler and assembler will function perfectly together.
/// 
/// All code written outside the context of a function is considered a global.
/// All globals are treated as labels.
/// </summary>
class Compiler : public ParserBase {
public:
	Compiler();
	~Compiler();

	bool compileFile(const std::string& fileName, std::ostream& outputStream);

protected:

	void parseFile();
	bool readComment();
	bool readOneTwoChar();

	void getSymbolInfo(int modifiers, SymbolInfo& info);

	/// <summary>
	/// Returns a type from the next set of tokens. True if success, false otherwise.
	/// </summary>
	/// <param name="typeInfo"></param>
	/// <returns></returns>
	bool getTypeFromToken(SymbolInfo& typeInfo);

	/// <summary>
	/// Gets the next variable from the tokens.
	/// Stores it on the current scope.
	/// </summary>
	/// <returns></returns>
	void parseVariableDeclaration(int modifiers, SymbolInfo*& symbolInfo, std::vector<SymbolInfo*>* args = nullptr);

	/// <summary>
	/// Parses a function definition and leaves the state right before the code block.
	/// The function defintion should be logged in the current symbol stack, then
	/// the code should be generated.
	/// </summary>
	/// <param name="name"></param>
	/// <param name="returnType"></param>
	/// <param name="args"></param>
	/// <returns></returns>
	void parseFunctionDefinition(int modifiers, SymbolInfo*& symbolInfo);

	/// <summary>
	/// A code segment starts with curly braces, and ends with a closing one.
	/// A new scope is pushed, then closed at the end.
	/// Code segments may be embedded within another one.
	/// The return indicates the number of stack frames to jump out of.
	/// </summary>
	int parseCodeSegment(SymbolInfo*& currentFunctions, int scopeLevel, int scopeReturnAddress);

	/// <summary>
	/// Writes an instruction to the output buffer, or increments the offset appropriately.
	/// </summary>
	/// <param name="instruction"></param>
	/// <param name="immediateFlag"></param>
	/// <param name="immediate"></param>
	virtual void writeInstruction(int instruction, bool immediateFlag = false, int immediate = 0) {
		if (pass > 1 && !errorsFound) {
			uint32_t* instructions = (uint32_t*)output;
			instructions[currentBinaryOffset / sizeof(uint32_t)] = instruction;
			currentBinaryOffset += sizeof(uint32_t);

			if (immediateFlag) {
				instructions[currentBinaryOffset / sizeof(uint32_t)] = immediate;
				currentBinaryOffset += sizeof(uint32_t);
			}
		}
		else {
			currentBinaryOffset += sizeof(uint32_t);
			if (immediateFlag) {
				currentBinaryOffset += sizeof(uint32_t);
			}
		}
	}

	/// <summary>
	/// Writes the variable to the code. 
	/// Datasegment just allocates a set amount of space then aligns it 
	/// to a word boundary.
	/// As args, we just write instructions to subtract n bytes from the stack pointer,
	/// word align it, then copy it in
	/// As a local variable, we just subtract n bytes from sp, then
	/// copy in the value they assigned if they provided one.
	/// If it's a function, do nothing..! we don't have to allocate space for exe code.
	/// </summary>
	void allocateVariable(const SymbolInfo& symbolInfo);

	/// <summary>
	///  Total size must be a multiple of 4 to stay on a word boundary.
	/// </summary>
	void writeSymbolToDataSegment(int totalSize, const SymbolInfo& symbolInfo) {
		if (pass > 1) {
			// Zero out the memory on disk.
			// I don't think it ever qualifies as bad behavior to give a zero intial value.
			// Random memory is usually worse than zero memory.
			std::memset(output + symbolInfo.value, 0, totalSize);
		}

		// increment the file offset.
		// We already know it's on a word boundary as defined by the requirements of the function call.
		currentBinaryOffset += totalSize;
	}

	/// <summary>
	/// Writes an OBJ header to the file just before the binary.
	/// </summary>
	/// <returns></returns>
	void writeOBJHeader(std::ostream& outputStream);

	/// <summary>
	/// Determines the value of a couple of terms.
	/// If its constant, it should be able to evaluate at compile time.
	/// </summary>
	void parseConstantExpression(ConstantValue& value, SymbolInfo& targetSymbolInfo, bool allowRefs);
	void parseConstantTerm(ConstantValue& value, SymbolInfo& targetSymbolInfo, bool allowRefs);
	void parseConstantFactor(ConstantValue& value, SymbolInfo& targetSymbolInfo, bool allowRefs);
	void parseMulOp(Operator& op);
	void parseAddOp(Operator& op);
	void parseCompareOp(Operator& op);
	void parseConst(SymbolInfo*& value, bool allowRefs);
	bool parseConstantRef(ConstantValue& value, SymbolInfo& symbolInfo, bool allowRefs);
	void parseConstantArrayInitializer(SymbolInfo& symbolInfo, int previousOffset, int currentDim);
	void getConstantAddressOfSymbol(const std::string& symbolName, ConstantValue& value, SymbolInfo& symbolInfo, bool allowRefs);
	bool isMulop();
	bool isAddop();
	bool isCompareOp();

	/// <summary>
	/// Parses a simple expression and writes the necessary instructions
	/// to the disk.
	/// </summary>
	/// <param name="info"></param>
	void parseExpression(SymbolInfo*& info);
	void parseTerm(SymbolInfo*& info);
	void parseFactor(SymbolInfo*& info);
	bool getValueFromToken(SymbolInfo*& info);
	void parseRef(SymbolInfo& info);
	void parseSizeOf(SymbolInfo*& info);
	void parseDeref(SymbolInfo*& info);
	void getAddressOfSymbol(const std::string& idtName, SymbolInfo& info);
	void parseIdentifierValue(SymbolInfo*& info);

	/// <summary>
	/// Returns whether it successfully pulled a constant
	/// from the code. Result is in the value.
	/// </summary>
	bool getConstantValueFromToken(ConstantValue& value, SymbolInfo& targetSymbolInfo, bool allowRefs);
	bool getConstantValueFromIdentifier(ConstantValue& value, SymbolInfo& targetSymbol, bool allowRefs);

	/// <summary>
	/// Performs an operator and stores the result in the final type. Uses static cast tables.
	/// </summary>
	/// <param name="left"></param>
	/// <param name="right"></param>
	/// <param name="dest"></param>
	/// <param name="operatorCode"></param>
	void performOperatorOnConstants(ConstantValue& left, ConstantValue& right, ConstantValue& dest, Operator op);

	/// <summary>
	/// Negative numbers can no longer exist alone in source code.
	/// </summary>
	/// <returns></returns>
	virtual bool collectNextToken() {
		trimWhiteSpace();

		if (!isEndOfStream()) {
			// Read the keyword, identifier, constant, or valid symbol
			if (isAlpha(buffer[bufferIndex])) {
				readIdentifierOrKeyword();
			}
			else if (isNum(buffer[bufferIndex])) {
				readNumber();
			}
			else if (buffer[bufferIndex] == '"') {
				readString();
			}
			else if (readOneTwoChar()) {
				bufferIndex++;
			}
			else {
				// An error occurs in this case
				endOfParse = true;
				errorsFound = true;
				currentToken.lexeme = buffer[bufferIndex];
				currentToken.code = UNDEFINED_CODE;
				addError("Unrecognized token");
				return false;
			}
		}
		else {
			// Terminate the program
			endOfParse = true;
			currentToken.lexeme = buffer[bufferIndex];
			currentToken.code = UNDEFINED_CODE;
			return false;
		}

		return true;
	}

	/// <summary>
	/// Returns an integer based on the next token.
	/// </summary>
	uint32_t tokenTo32BitInt() {
		int ret = 0;
		int base = 10;

		if (currentToken.code == INTEGER_CODE) {
			base = 10;
		}
		else if (currentToken.code == HEX_INTEGER_CODE) {
			base = 16;
		}
		else if (currentToken.code == OCTAL_INTER_CODE) {
			base = 8;
		}
		else if (currentToken.code == BINARY_INTEGER_CODE) {
			base = 2;
		}

		try {
			ret = std::stoi(currentToken.lexeme, 0, base);
			return ret;
		}
		catch (std::exception&) {
			addError("Failed to parse integer");
		}
		return 0;
	}

	float tokenTo32BitFloat() {
		float ret = 0;

		try {
			ret = std::stof(currentToken.lexeme);
			return ret;
		}
		catch (std::exception&) {
			addError("Failed to parse float");
		}

		return 0;
	}

	void getConstantFromDataSegment(ConstantValue& dest, SymbolInfo& info);
	void writeConstantValueToDataSegment(ConstantValue& value, SymbolInfo& symbolInfo);

	void writeLoadValueFromAddressInstructions(SymbolInfo& info, int offset, int addressReg, int destReg);
	void writeStoreValueInAddressInstructions(SymbolInfo& info, int offset, int addressReg, int valueReg);
	/// Writes instructions to load an address and returns an offset to it.
	int writeLoadAddressInstructions(SymbolInfo& info, int registerToUse);

	/// <summary>
	/// Reads a value into a set register.
	/// If it's a global, load the address, then the value
	/// If its an immediate, perform a mov operator
	/// If it's a local variable, using the offset from BP, load the value
	/// </summary>
	/// <param name="info"></param>
	/// <param name="registerToUse"></param>
	void writeLoadValueInstructions(SymbolInfo& info, int registerToUse);
	void writeLoadValueOrAddressInstructions(SymbolInfo& value, int registerToUse);

	void writeOperatorInstructions(SymbolInfo& left, SymbolInfo& right, Operator op);
	void writeCompareOperatorInstructions(SymbolInfo& left, SymbolInfo& right, Operator op);
	void writeAssignment(SymbolInfo& left, SymbolInfo& right);

protected:
	SymbolStack symbolStack;
	TypeTable types;
	std::map<int, Operator> tokenCodeToOperator;

	friend class SymbolStack;
	friend class LocalSymbolTable;
	friend class GlobalSymbolTable;

	void staticCast(TypeCode startType, TypeCode endType, ConstantValue& value);
};
