#pragma once

#include <stdint.h>
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "../DebugLogger.h"

constexpr int UNDEFINED_CODE = 0x7FFFFFFF;
constexpr int IDENTIFIER_CODE = 100;
constexpr int STRING_CODE = 101;
constexpr int BINARY_INTEGER_CODE = 103;
constexpr int OCTAL_INTER_CODE = 104;
constexpr int HEX_INTEGER_CODE = 105;
constexpr int INTEGER_CODE = 106;
constexpr int FLOAT_CODE = 107;

/// <summary>
/// Class holding a token and information associated with it.
/// </summary>
class Token {
public:
	Token() :
		lexeme(""),
		code(0)
	{
	}

	std::string lexeme;
	int code;
};

/// <summary>
/// Designed to compare two strings without checking case.
/// This can be overloaded in the cpp file because you won't be linking the 
/// compiler with the assembler. Link time overloading.... lmao, terrible design.
/// </summary>
struct ReserveTableCompare
{
	bool operator ()(const std::string& a, const std::string& b) const;
};

/// <summary>
/// Class representing all reserves for this assembly language.
/// </summary>
class ReserveTable {
public:
	ReserveTable() {
	}

	/// <summary>
	/// Adds to the table a new reserve word (one which is prioritized during the assembly 
	/// process before immediates.
	/// </summary>
	/// <param name="word"></param>
	/// <param name="code"></param>
	int addReserveWord(const std::string& word, int code) {
		reserveTable.emplace(word, code);
		return code;
	}

	/// <summary>
	/// Returns the code of a reserve word if it exists in the table,
	/// otherwise -1.
	/// </summary>
	/// <param name="word"></param>
	/// <returns></returns>
	int getReserveCode(const std::string& word) {
		std::map<std::string, int>::const_iterator foundToken = reserveTable.find(word);

		if (foundToken != reserveTable.end()) {
			return foundToken->second;
		}

		return -1;
	}

private:
	std::map<std::string, int, ReserveTableCompare> reserveTable;
};

/// <summary>
/// The various types of symbols that may exist.
/// </summary>
enum SymbolType {
	UNDEFINED,
	FLOAT_CONSTANT,
	CONSTANT,
	LOCAL,
	EXPORTED,
	IMPORTED,
	VAR,
	ARGUMENT,
	FUNCTION
};

/// <summary>
/// Holds the local state for symbols.
/// </summary>
class SymbolTable {
public:
	SymbolTable() :
		exportedSymbolCount(0)
	{
		clear();
	}

	void clear() {
		exportedSymbolCount = 0;

		symbolTypes.clear();
		symbolValue.clear();
		symbolImportReferencesTextSeg.clear();
		symbolImportReferencesDataSeg.clear();
		globalOffsetTable.clear();
	}

	bool addSymbol(const std::string& symbol, SymbolType type, int value) {
		if (symbolTypes.find(symbol) == symbolTypes.end()) {
			if (type == SymbolType::EXPORTED) {
				exportedSymbolCount++;
			}

			symbolTypes[symbol] = type;
			symbolValue[symbol] = value;
			return true;
		}
		else {
			return false;
		}
	}

	SymbolType getSymbolType(const std::string& symbol) {
		std::map<std::string, SymbolType>::iterator foundSymbol = symbolTypes.find(symbol);

		if (foundSymbol != symbolTypes.end()) {
			return foundSymbol->second;
		}

		return UNDEFINED;
	}

	int getSymbolValue(const std::string& symbol) {
		std::map< std::string, int>::iterator foundSymbol = symbolValue.find(symbol);

		if (foundSymbol != symbolValue.end()) {
			return foundSymbol->second;
		}

		return 0;
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

	void addLocalDataSegmentReference(const std::string& symbol, int binaryOffset) {
		globalOffsetTable.push_back(binaryOffset);
	}

	// Maps a symbol name to a SymbolType.
	std::map<std::string, SymbolType> symbolTypes;
	std::map<std::string, int> symbolValue;
	std::map<std::string, std::vector<int>> symbolImportReferencesTextSeg;
	std::map<std::string, std::vector<int>> symbolImportReferencesDataSeg;
	std::vector<int> globalOffsetTable;

	int exportedSymbolCount = 0;
};


/// <summary>
/// List of instructions, must appear in the order of their opcodes.
/// </summary>
enum Instructions {
	NOP,
	LSL,
	LSR,
	ADD,
	SUB,
	MUL,
	DIV,
	ULSL,
	ULSR,
	UADD,
	USUB,
	AND,
	OR,
	XOR,
	MOV,
	CMP,
	LA,
	MOV_GE,
	MOV_G,
	MOV_LE,
	MOV_L,
	MOV_E,
	MOV_NE,
	MOV_C,
	MOV_ITOF,
	MOV_FTOI,
	FADD,
	FSUB,
	FMUL,
	FDIV,
	FCMP,
	FMOV,
	FS,
	FL,
	SB,
	SH,
	SW,
	LB,
	LH,
	LW,
	JMP,
	CALL,
	JGE,
	JG,
	JLE,
	JL,
	JE,
	JNE,
	JC,
	PUSHB,
	POPB,
	SWI,
	HWI,
	MOD,
	MOV_UITOF,
	PUSHH,
	PUSHW,
	POPH,
	POPW,
	PUSHF,
	POPF,
	INC,
	DEC,
	RET, // = JMP RA
	INSTRUCTION_COUNT
};

/// <summary>
/// Base class to handle basic operations of a parser.
/// </summary>
class ParserBase {
public:
	ParserBase() :
		output(nullptr),
		buffer(nullptr),
		bufferIndex(0),
		bufferSize(0),
		currentLineNumber(0),
		reserveTable(),
		logger("", Level::LEVEL_TRACE),
		endOfParse(false),
		pass(0),
		errorsFound(false),
		currentBinaryOffset(0)
	{
	}

protected:
	virtual void parseFile() = 0;

	/// <summary>
	/// Outputs an error to the console giving the token where it broke,
	/// and the line number.
	/// </summary>
	/// <param name="text"></param>
	void addError(const std::string& text) {
		logger.error("error: {s} on line {d}: '{s}']", text.c_str(), currentLineNumber + 1, currentToken.lexeme.c_str());
		errorsFound = true;
	}

	/// <summary>
	/// Adds a warning which can be ignored.
	/// </summary>
	/// <param name="text"></param>
	void addWarning(const std::string& text) {
		logger.warning("error: {s} on line {d}: '{s}']", text.c_str(), currentLineNumber + 1, currentToken.lexeme.c_str());
	}

	/// <summary>
	/// Writes an instruction to the output buffer, or increments the offset appropriately.
	/// </summary>
	/// <param name="instruction"></param>
	/// <param name="immediateFlag"></param>
	/// <param name="immediate"></param>
	virtual void writeInstruction(int instruction, bool immediateFlag = false, int immediate = 0) {
		if (pass != 0 && !errorsFound) {
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

	bool isEndOfStream() {
		return bufferIndex >= bufferSize || endOfParse;
	}

	/// <summary>
	/// Returns whether a character counts as white space for this parser
	/// </summary>
	/// <param name="character"></param>
	/// <returns></returns>
	bool isWhiteSpace(char character) {
		return character == ' ' || character == '\n' || character == '\r' || character == '\t';
	}

	/// <summary>
	/// Returns whether the character is alpha or not
	/// </summary>
	/// <param name="character"></param>
	/// <returns></returns>
	bool isAlpha(char character) {
		return (character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z');
	}

	/// <summary>
	/// Returns whether the character is a number or not.
	/// </summary>
	/// <param name="character"></param>
	/// <returns></returns>
	bool isNum(char character) {
		return character >= '0' && character <= '9';
	}

	/// <summary>
	/// Reads from the file either an identifier or keyword based on the reserve table.
	/// </summary>
	virtual void readIdentifierOrKeyword() {
		char* lexemeStart = buffer + bufferIndex;

		// If the first character is alpha, we can move onto the next
		while (!isEndOfStream() &&
			(isAlpha(buffer[bufferIndex]) || 
			isNum(buffer[bufferIndex]) ||
			buffer[bufferIndex] == '_')) 
		{
			bufferIndex++;
		}

		// Check if its in the reserve table
		currentToken.lexeme = std::string(lexemeStart, buffer + bufferIndex);

		int reserveTableCode = reserveTable.getReserveCode(currentToken.lexeme);

		if (reserveTableCode != -1) {
			currentToken.code = reserveTableCode;
		}
		else {
			currentToken.code = IDENTIFIER_CODE;
		}
	}

	/// <summary>
	/// Reads a number from the stream.
	/// </summary>
	virtual void readNumber() {
		char* lexemeStart = buffer + bufferIndex;

		if (buffer[bufferIndex] == '-') {
			bufferIndex++;
		}

		bool floatingPoint = false;

		// Load a hex number if that's what this is
		if (!isEndOfStream() && buffer[bufferIndex] == '0') {
			if (buffer[bufferIndex + 1] == 'b') {
				// Load binary.
				bufferIndex += 2;
				while (!isEndOfStream() && buffer[bufferIndex] == '0' || buffer[bufferIndex] == '1') {
					bufferIndex++;
				}
				currentToken.lexeme = std::string(lexemeStart, buffer + bufferIndex);
				currentToken.code = BINARY_INTEGER_CODE;
				return;
			}
			else if (buffer[bufferIndex + 1] == 'x') {
				// Load hex.
				bufferIndex += 2;
				while (!isEndOfStream() && (buffer[bufferIndex] >= '0' && buffer[bufferIndex] <= '9') ||
					(buffer[bufferIndex] >= 'A' && buffer[bufferIndex] <= 'F') ||
					buffer[bufferIndex] >= 'a' && buffer[bufferIndex] <= 'f') {
					bufferIndex++;
				}

				currentToken.lexeme = std::string(lexemeStart, buffer + bufferIndex);
				currentToken.code = HEX_INTEGER_CODE;
				return;
			}
			else if (buffer[bufferIndex + 1] == 'o') {
				// Load octal.
				bufferIndex += 2;
				while (!isEndOfStream() && buffer[bufferIndex] >= '0' && buffer[bufferIndex] <= '7') {
					bufferIndex++;
				}
				currentToken.lexeme = std::string(lexemeStart, buffer + bufferIndex);
				currentToken.code = OCTAL_INTER_CODE;
				return;
			}
		}

		// Assume it's an integer until we see one of a few things:
		// A decimal point turns it into a float
		// e/E for expoential notation turns it into a float
		while (!isEndOfStream() && isNum(buffer[bufferIndex])) {
			bufferIndex++;
		}

		// When its not a number anymore, we must see what it is!
		if (!isEndOfStream()) {
			if (buffer[bufferIndex] == 'e' || buffer[bufferIndex] == 'E') {
				bufferIndex++;

				// Optional negative on exponent
				if (!isEndOfStream() && buffer[bufferIndex] == '-') {
					bufferIndex++;
				}

				// Load the rest of the number.
				if (!isEndOfStream() && isNum(buffer[bufferIndex])) {
					while (!isEndOfStream() && isNum(buffer[bufferIndex])) {
						bufferIndex++;
					}
				}

				floatingPoint = true;
			}
			else if (buffer[bufferIndex] == '.') {
				bufferIndex++;
				if (!isEndOfStream()) {
					// Load the rest of the number.
					while (!isEndOfStream() && isNum(buffer[bufferIndex])) {
						bufferIndex++;
					}
				}

				floatingPoint = true;
			}
		}

		currentToken.lexeme = std::string(lexemeStart, buffer + bufferIndex);
		if (floatingPoint) {
			currentToken.code = FLOAT_CODE;
		}
		else {
			currentToken.code = INTEGER_CODE;
		}
	}

	/// <summary>
	/// Reads a comment
	/// </summary>
	/// <returns>True if you read a comment.</returns>
	virtual bool readComment() = 0;

	/// <summary>
	/// Removes the whitespace from the file.
	/// </summary>
	virtual void trimWhiteSpace() {
		while (!isEndOfStream() && isWhiteSpace(buffer[bufferIndex])) {
			if (buffer[bufferIndex] == '\n') {
				currentLineNumber++;
			}

			bufferIndex++;
		}

		if (readComment()) {
			trimWhiteSpace();
		}
	}

	/// <summary>
	/// Reads a token which consists of just a few characters.
	/// </summary>
	/// <returns></returns>
	virtual bool readOneTwoChar() = 0;

	/// <summary>
	/// Reads a string token from the stream.
	/// </summary>
	virtual void readString() {
		bufferIndex++;
		currentToken.lexeme = "";

		while (!isEndOfStream() && buffer[bufferIndex] != '"' && buffer[bufferIndex] != '\r' && buffer[bufferIndex] != '\n') {
			if (buffer[bufferIndex] == '\\') {
				bufferIndex++;
				if (buffer[bufferIndex] == 't') {
					currentToken.lexeme += '\t';
				}
				else if (buffer[bufferIndex] == 'b') {
					currentToken.lexeme += '\b';
				}
				else if (buffer[bufferIndex] == 'n') {
					currentToken.lexeme += '\n';
				}
				else if (buffer[bufferIndex] == '\\') {
					currentToken.lexeme += '\\';
				}
			}
			else {
				currentToken.lexeme += buffer[bufferIndex];
			}

			bufferIndex++;
		}

		currentToken.code = STRING_CODE;

		if (buffer[bufferIndex] != '"') {
			addWarning("String ran off the end of the line without closing quote, may cause unexpected behavior");
		}

		bufferIndex++;
	}

	/// <summary>
	/// Reads from the file buffer the next token and stores it in the current token variable.
	/// </summary>
	virtual bool collectNextToken() {
		trimWhiteSpace();

		if (!isEndOfStream()) {
			// Read the keyword, identifier, constant, or valid symbol
			if (isAlpha(buffer[bufferIndex])) {
				readIdentifierOrKeyword();
			}
			else if (isNum(buffer[bufferIndex]) || buffer[bufferIndex] == '-') {
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

	Token currentToken;
	ReserveTable reserveTable;
	DebugLogger logger;
	char* buffer;
	int bufferIndex;
	bool endOfParse = false;
	int bufferSize;
	int currentLineNumber;
	int pass = 0;
	bool errorsFound = false;
	int currentBinaryOffset = 0;
	char* output;
	int entryPoint = -1;
};

/// <summary>
/// Class responsible for creating a binary file from an input file
/// </summary>
class Assembler : public ParserBase {
public:
	Assembler();

	/// <summary>
	/// Assembles a file and returns a newly allocated output buffer.
	/// </summary>
	/// <param name="fileName"></param>
	/// <param name="outputBuffer"></param>
	/// <param name="outputBufferSize"></param>
	bool assembleFile(const std::string& fileName, std::ostream& outputFile);

protected:
private:
	/// <summary>
	/// Parses the entire file checking if it matches the syntax.
	/// </summary>
	void parseFile();

	/// <summary>
	/// Reads a single valid character from the file.
	/// </summary>
	/// <return>True if and only if the single char in the file is valid for this parser.</return>
	bool readOneTwoChar();

	/// <summary>
	/// Reads an int register from the file and stores it in register index
	/// </summary>
	/// <param name="registerIndex"></param>
	/// <returns></returns>
	bool readIntRegister(int& registerIndex);
	bool readFloatRegister(int& registerIndex);

	/// <summary>
	/// Comments begin with a semi colon and end at a new line.
	/// </summary>
	/// <returns></returns>
	bool readComment() {
		// Enter into a comment till the end of the line
		if (buffer[bufferIndex] == ';') {
			while (!isEndOfStream() && buffer[bufferIndex] != '\n') {
				bufferIndex++;
			}

			return true;
		}

		return false;
	}

	/// <summary>
	/// Uses the next token to try and load an integer.
	/// If labels are allowed and its imported, importReferenceOffset
	/// Specifies the offset from the currentBinaryOffset that symbol should
	/// be changed during the linking process. For all instructions, its
	/// CurrentBinaryOffset + 4
	/// </summary>
	/// <param name="value"></param>
	/// <param name="acceptLabels"></param>
	/// <param name="importReferenceOffset"></param>
	/// <returns></returns>
	bool getIntegerFromToken(int& value, bool acceptLabels, bool dataSegment);
	bool getFloatFromToken(float& value);

	void parseLabel();
	void parseExport();
	void parseImport();
	void parseSegment();
	void parseDataSegment();
	void parseTextSegment();
	void parseConst();
	void parseByte();
	void parseHalf();
	void parseWord();
	void parseFloat();
	void parseAscii(bool nullTerminated);
	void parseSpace();
	void parseNoArgs(int opcode);
	void parseThreeArgsInt(int opcode);
	void parseTwoArgsInt(int opcode);
	void parseIntToFloat(int opcode);
	void parseFloatToInt(int opcode);
	void parseThreeArgsFloat(int opcode);
	void parseTwoArgsFloat(int opcode);
	void parseStoreLoad(int opcode);
	void parseJump(int opcode);
	void parsePushPop(int opcode);
	void parseIncDec(int opcode);
	void parseSwi(int opcode);

	void writeOBJHeader(std::ostream& outputStream);

	SymbolTable symbolTable;
};
