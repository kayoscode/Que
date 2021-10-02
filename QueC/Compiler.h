#pragma once

#include <string>
#include <map>
#include <vector>
#include <cassert>

#include "../QueASM/Assembler.h"

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

class Compiler;
#define MAX_CONSTANT_VALUE_SIZE 8

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

enum Register {
	R0, R1, R2, R3,
	R4, R5, R6, R7,
	R8, R9, R10, R11,
	R12, R13, R14, R15,
	PC, BP, SP, FR, RA,
	INT_REGISTER_COUNT
};

enum ModifierFlags {
	EXPORT_FLAG = 1,
	EXTERN_FLAG = 2,
	ARGUMENT_FLAG = 4,
	ENTRYPT_FLAG = 8,
	LOCAL_VAR_FLAG = 16,
};

enum class Operator {
	MULT,
	DIV,
	ADD,
	SUB
};

enum class TypeCode {
	UNDEFINED,
	TYPE_INT8_CODE,
	TYPE_INT16_CODE,
	TYPE_INT32_CODE,
	TYPE_BOOL_CODE,
	TYPE_FLOAT32_CODE,
	CONSTANT_TYPE_COUNT
};

/// <summary>
/// Includes all information attached to a typed symbol.
/// </summary>
struct TypeInfo {
	int baseSize = 0;
	TypeCode typeCode = TypeCode::UNDEFINED;
	int ptrCount = 0;

	int sizeInMemory() {
		if (ptrCount > 0) {
			return 4;
		}

		return baseSize;
	}
};

/// <summary>
/// Holds data for a constant value.
/// </summary>
struct ConstantValue {
	// Reserved 8 chars, even though we will only ever need 4
	char value[MAX_CONSTANT_VALUE_SIZE] = { 0 };
	TypeInfo type;
};

/// <summary>
/// Stores a temporary value in code.
/// Unlike constant value, this is mutated through code, rather
/// than static manipulation.
/// A register index is stored as well as type information
/// </summary>
struct ParseValue {
	int variableCode = 0;
	TypeInfo type;
};

/// <summary>
/// Data defined for a type
/// </summary>
struct TypeDef {
	TypeCode code = TypeCode::UNDEFINED;
	int size = -1;
};

enum class QueSymbolAccessType {
	UNDEFINED,
	LOCAL,
	IMPORTED,
	EXPORTED,
	ARGUMENT,
	VARIABLE,
	COMPILER_ACCESS
};

enum class QueSymbolType {
	UNDEFINED,
	DATA,
	FUNCTION,
	CONST_EXPR,
	ARRAY
};

/// <summary>
/// Table storing types and their various sizes.
/// Soon we will have to deal with a type stack, but we can handle that later.
/// </summary>
class TypeTable {
public:
	TypeTable() :
		typeData() 
	{
	}

	TypeCode addType(const std::string& typeName, int size, TypeCode code) {
		TypeDef def;
		def.code = code;
		def.size = size;
		typeCodes.emplace(typeName, code);
		typeData.emplace(code, def);
		return code;
	}

	bool typeExists(const std::string& typeName) {
		return typeCodes.find(typeName) != typeCodes.end();
	}

	bool typeExists(TypeCode code) {
		return typeData.find(code) != typeData.end();
	}

	int getTypeSize(const std::string& typeName) {
		return typeData[typeCodes[typeName]].size;
	}

	TypeCode getTypeCode(const std::string& typeName) {
		return typeCodes[typeName];
	}

	int getTypeSize(TypeCode code) {
		return typeData[code].size;
	}

private:
	std::map<TypeCode, TypeDef> typeData;
	std::map<std::string, TypeCode> typeCodes;
};

struct SymbolInfo {
	QueSymbolAccessType accessType = QueSymbolAccessType::UNDEFINED;
	QueSymbolType symbolType = QueSymbolType::UNDEFINED;
	TypeInfo typeInfo;
	int value = 0;
	std::string name;

	// For array types, they have n dimensions, and n sizes per dimension
	std::vector<int> dimensions;
	std::vector<int> totalDimensionSizes;
	int totalElementCount;

	// only applicable to function symbols.
	// Contains a list of all information about its arguments.
	std::vector<SymbolInfo> fnArgs;
	// Used for functions, says whether the symbol has been implemented or not in this assembly.
	// Used for globals if the variable has been initialized (in pass2)
	bool defined = false;

	void calculateTotalDimensionElements() {
		int previousDimensionSize = 1;
		totalDimensionSizes.resize(dimensions.size());

		for (int i = dimensions.size() - 1; i >= 0; i--) {
			previousDimensionSize = previousDimensionSize * dimensions[i];
			totalDimensionSizes[i] = previousDimensionSize;
		}
	}
};

/// <summary>
/// Local symbol tables don't have to worry about things like imported
/// and exported symbols.
/// </summary>
class LocalSymbolTable {
public:
	const int INITIAL_OFFSET_FROM_BP = 0;

	LocalSymbolTable()
	{
		clear();
		// This table is referring to local variables. 
		// Based on this calling convention, when we enter a new stack frame,
		// the base pointer comes right before RA, meaning its offset
		// by 4 bytes from the first stack push.
		// When adding a variable to the stack frame,
		// subtract the size first, then copy the value in at the current SP
		currentOffsetFromBP = INITIAL_OFFSET_FROM_BP;
	}

	void clear() {
		symbolInfo.clear();
		symbolCodes.clear();
		currentOffsetFromBP = INITIAL_OFFSET_FROM_BP;
	}

	int addSymbol(const std::string& symbol, const SymbolInfo& info) {
		if (symbolCodes.find(symbol) == symbolCodes.end()) {
			symbolCodes[symbol] = symbolCodeIndex;
			symbolInfo[symbolCodeIndex] = info;
			return symbolCodeIndex++;
		}
		else {
			return 0;
		}
	}

	int getSymbolInfo(const std::string& symbol, SymbolInfo& info) {
		std::map<std::string, int>::iterator foundSymbol = symbolCodes.find(symbol);

		if (foundSymbol != symbolCodes.end()) {
			info = symbolInfo[foundSymbol->second];
			return foundSymbol->second;
		}

		return 0;
	}

	bool setSymbolValue(const std::string& symbol, int value) {
		std::map<std::string, int>::iterator foundSymbol = symbolCodes.find(symbol);

		if (foundSymbol != symbolCodes.end()) {
			symbolInfo[foundSymbol->second].value = value;
			return foundSymbol->second;
		}

		return false;
	}

	/// <summary>
	/// Size should be a multiple of 4 to stay on a word boundary.
	/// </summary>
	/// <param name="compiler"></param>
	/// <param name="size"></param>
	void pushLocalVariable(Compiler* compiler, int size);

	int getLocalBPOffset() {
		return currentOffsetFromBP - INITIAL_OFFSET_FROM_BP;
	}

	// Maps a symbol name to some info about it.
	std::map<int, SymbolInfo> symbolInfo;
	std::map<std::string, int> symbolCodes;
	int currentOffsetFromBP = 0;

	// Register management
	bool registerInUse[INT_REGISTER_COUNT] = { false };
	std::map<std::string, Register> variableRegisterAllocation;

	// Should be called right before a register is used.
	void allocateRegister(int variable);
	void freeRegister(int variable);

	// same for floats
	int symbolCodeIndex = 1;
};

class GlobalSymbolTable {
public:
	GlobalSymbolTable() {
		clear();
	}

	void clear() {
		globalOffsetTable.clear();
		exportedSymbolCount = 0;
		symbolInfo.clear();
		symbolImportReferencesDataSeg.clear();
		symbolImportReferencesTextSeg.clear();
		symbolCodeIndex = 0;
	}

	bool addSymbol(const std::string& name, const SymbolInfo& info) {
		std::map<std::string, SymbolInfo>::iterator foundSymbol =
			symbolInfo.find(name);

		if (foundSymbol == symbolInfo.end()) {
			symbolInfo.emplace(name, info);

			if (info.accessType == QueSymbolAccessType::EXPORTED) {
				exportedSymbolCount++;
			}

			return true;
		}

		return false;
	}

	bool getSymbolInfo(const std::string& name, SymbolInfo& info) {
		std::map<std::string, SymbolInfo>::iterator foundSymbol =
			symbolInfo.find(name);

		if (foundSymbol != symbolInfo.end()) {
			info = symbolInfo[name];
			return true;
		}
		return false;
	}

	void addImportReferenceTextSeg(const std::string& symbol, int binaryOffset) {
		if (symbolImportReferencesTextSeg.find(symbol) == symbolImportReferencesTextSeg.end()) {
			symbolImportReferencesTextSeg.emplace(symbol, std::vector<int>());
		}

		symbolImportReferencesTextSeg[symbol].push_back(binaryOffset);
	}

	void addImportReferenceDataSeg(const std::string& symbol, int binaryOffset) {
		if (symbolImportReferencesDataSeg.find(symbol) == symbolImportReferencesDataSeg.end()) {
			symbolImportReferencesDataSeg.emplace(symbol, std::vector<int>());
		}

		symbolImportReferencesDataSeg[symbol].push_back(binaryOffset);
	}

	void addLocalDataSegmentReference(int binaryOffset) {
		globalOffsetTable.push_back(binaryOffset);
	}

	void setSymbolDefined(const std::string& symbolName) {
		symbolInfo[symbolName].defined = true;
	}

	void setSymbolValue(const std::string& symbolName, int value) {
		symbolInfo[symbolName].value = value;
	}

	std::map<std::string, SymbolInfo> symbolInfo;
	std::map<std::string, std::vector<int>> symbolImportReferencesTextSeg;
	std::map<std::string, std::vector<int>> symbolImportReferencesDataSeg;
	int exportedSymbolCount;
	std::vector<int> globalOffsetTable;
	int symbolCodeIndex = 0;
};

class RegisterManager;

/// <summary>
/// Holds information about a symbol scope.
/// </summary>
class SymbolStack {
public:
	SymbolStack(Compiler* compiler) {
		this->compiler = compiler;
	}

	~SymbolStack() {
		// Delete each ptr.
		for (int i = 0; i < localScope.size(); i++) {
			delete localScope[i];
		}
	}

	int getCurrentBPOffset() {
		assert(!atGlobalScope());
		return localScope[scopeIndex]->getLocalBPOffset();
	}

	bool searchSymbol(const std::string& symbol, SymbolInfo& symbolInfo) {
		for (int i = 0; i < localScope.size(); i++) {
			if (localScope[i]->getSymbolInfo(symbol, symbolInfo)) {
				return true;
			}
		}
		// TODO: add any references to the GOT and or import/export refernces for the Global symbols

		// If we cant find it in any local scope, search for it in the global scope.
		if (globalSymbols.getSymbolInfo(symbol, symbolInfo)) {
			return true;
		}
		
		// If we are on the first pass, not having symbol information isn't a problem.
		return false;
	}

	void addSymbolToCurrentScope(const SymbolInfo& info) {
		if (scopeIndex >= 0) {
			// Add to a local scope
			if (!localScope[scopeIndex]->addSymbol(info.name, info)) {
				// TODO: variable declared multiple times. big nono.
			}
		}
		else {
			// Add it to the global scope
			if (!globalSymbols.addSymbol(info.name, info)) {
				// TODO: variable declared multiple times in global scope, big nono.
			}
		}
	}

	/// <summary>
	/// Pushes a new scope.
	/// </summary>
	void pushScope() {
		scopeIndex++;
		if (scopeIndex >= localScope.size()) {
			localScope.push_back(new LocalSymbolTable());
		}
		//localScope[localScope.size() - 1]->symbolCodeIndex = 
	}

	/// <summary>
	/// Removes the previous scope from the list. Will not remove the global stack.
	/// </summary>
	bool popScope() {
		if (scopeIndex >= 0) {
			localScope[scopeIndex]->clear();
			scopeIndex--;
		}
		else {
			return false;
		}

		return true;
	}

	/// <summary>
	/// Sends us to the global scope, any identifiers written during this time will
	/// be written at the global level.
	/// </summary>
	void enterPreviousScope() {
		if (!atGlobalScope()) {
			if (saveScopeIndex == -1) {
				saveScopeIndex = scopeIndex;
			}

			scopeIndex--;
		}
	}

	/// <summary>
	/// If the scope is not pointing to the top, we will reenter it.
	/// </summary>
	void enterCurrentScope() {
		if (saveScopeIndex != -1) {
			scopeIndex = saveScopeIndex;
			saveScopeIndex = -1;
		}
	}

	bool atGlobalScope() {
		return scopeIndex < 0;
	}

	void pushVariable(const std::string& variable);

	int scopeIndex = -1;
	int saveScopeIndex = -1;
	std::vector<LocalSymbolTable*> localScope;
	GlobalSymbolTable globalSymbols;
	Compiler* compiler;
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
	void parseVariableDeclaration(int modifiers, SymbolInfo& symbolInfo, std::vector<SymbolInfo>* args = nullptr);

	/// <summary>
	/// Parses a function definition and leaves the state right before the code block.
	/// The function defintion should be logged in the current symbol stack, then
	/// the code should be generated.
	/// </summary>
	/// <param name="name"></param>
	/// <param name="returnType"></param>
	/// <param name="args"></param>
	/// <returns></returns>
	void parseFunctionDefinition(int modifiers, SymbolInfo& symbolInfo);

	/// <summary>
	/// A code segment starts with curly braces, and ends with a closing one.
	/// A new scope is pushed, then closed at the end.
	/// Code segments may be embedded within another one.
	/// </summary>
	void parseCodeSegment();

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
		if (pass > 0) {
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
	void parseConst(SymbolInfo& value, bool allowRefs);
	bool parseConstantRef(ConstantValue& value, SymbolInfo& symbolInfo, bool allowRefs);
	void parseConstantArrayInitializer(SymbolInfo& symbolInfo, int previousOffset, int currentDim);
	void getConstantAddressOfSymbol(const std::string& symbolName, ConstantValue& value, SymbolInfo& symbolInfo, bool allowRefs);

	/// <summary>
	/// Parses a simple expression and writes the necessary instructions
	/// to the disk.
	/// </summary>
	/// <param name="info"></param>
	void parseExpression(SymbolInfo& info);
	void parseTerm(SymbolInfo& info);
	void parseFactor(SymbolInfo& info);
	bool getValueFromToken(SymbolInfo& info);

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

	bool isMulop();
	bool isAddop();

protected:
	SymbolStack symbolStack;
	TypeTable types;
	std::map<int, Operator> tokenCodeToOperator;

	friend class SymbolStack;
	friend class LocalSymbolTable;
	friend class GlobalSymbolTable;

	void staticCast(TypeCode startType, TypeCode endType, ConstantValue& value);
};
