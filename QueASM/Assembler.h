#pragma once

#include <stdint.h>
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include "../DebugLogger.h"

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
/// Class responsible for creating a binary file from an input file
/// </summary>
class Assembler {
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
	/// Reads from the file buffer the next token and stores it in the current token variable.
	/// </summary>
	bool collectNextToken();
	
	/// <summary>
	/// Removes leading whitespace from the current position in the buffer while making sure 
	/// to not overflow.
	/// </summary>
	void trimWhiteSpace();

	/// <summary>
	/// Reads the next identifier or keyword from the file buffer.
	/// </summary>
	void readIdentifierOrKeyword();

	/// <summary>
	/// Reads a string from the file.
	/// </summary>
	void readString();

	/// <summary>
	/// Reads the next integer or floating point number from the file.
	/// </summary>
	void readNumber();

	/// <summary>
	/// Reads a single valid character from the file.
	/// </summary>
	/// <return>True if and only if the single char in the file is valid for this parser.</return>
	bool readOneChar();

	/// <summary>
	/// Reads an int register from the file and stores it in register index
	/// </summary>
	/// <param name="registerIndex"></param>
	/// <returns></returns>
	bool readIntRegister(int& registerIndex);
	bool readFloatRegister(int& registerIndex);

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

	enum SymbolType {
		UNDEFINED,
		FLOAT_CONSTANT,
		CONSTANT,
		LOCAL,
		EXPORTED,
		IMPORTED
	};

	/// <summary>
	/// Outputs an error to the console giving the token where it broke,
	/// and the line number.
	/// </summary>
	/// <param name="text"></param>
	void addError(const std::string& text) {
		logger.error("error: {s} on line {d}: '{s}']", text.c_str(), currentLineNumber + 1, currentToken.lexeme.c_str());
		errorsFound = true;
	}

	void addWarning(const std::string& text) {
		logger.warning("error: {s} on line {d}: '{s}']", text.c_str(), currentLineNumber + 1, currentToken.lexeme.c_str());
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
	bool getIntegerFromToken(int& value, bool acceptLabels);
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

	/// <summary>
	/// Writes an instruction to the output buffer, or increments the offset appropriately.
	/// </summary>
	/// <param name="instruction"></param>
	/// <param name="immediateFlag"></param>
	/// <param name="immediate"></param>
	void writeInstruction(int instruction, bool immediateFlag, int immediate) {
		if (pass == 1) {
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

	void writeOBJHeader(std::ostream& outputStream);

	struct ReserveTableCompare
	{
		bool operator ()(const std::string& a, const std::string& b) const;
	};

	/// <summary>
	/// Holds the local state for symbols.
	/// </summary>
	class SymbolTable {
	public:
		SymbolTable():
			exportedSymbolCount(0),
			importedSymbolCount(0)
		{
			clear();
		}

		void clear() {
			exportedSymbolCount = 0;
			importedSymbolCount = 0;

			symbolTypes.clear();
			symbolValue.clear();
		}

		bool addSymbol(const std::string& symbol, SymbolType type, int value) {
			if (symbolTypes.find(symbol) == symbolTypes.end()) {
				if (type == SymbolType::EXPORTED) {
					exportedSymbolCount++;
				}
				else if (type == SymbolType::IMPORTED) {
					importedSymbolCount++;
				}

				symbolTypes[symbol] = type;
				symbolValue[symbol] = value;
				symbolImportReferences.emplace(symbol, std::vector<int>());
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

		void addImportReference(const std::string& symbol, int binaryOffset)  {
			symbolImportReferences[symbol].push_back(binaryOffset);
		}

		// Maps a symbol name to a SymbolType.
		std::map<std::string, SymbolType> symbolTypes;
		std::map<std::string, int> symbolValue;
		std::map<std::string, std::vector<int>> symbolImportReferences;

		int exportedSymbolCount = 0;
		int importedSymbolCount = 0;
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
		void addReserveWord(const std::string& word, int code) {
			reserveTable.emplace(word, code);
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

	bool isEndOfStream() {
		return bufferIndex >= bufferSize || endOfParse;
	}

	Token currentToken;
	ReserveTable reserveTable;
	SymbolTable symbolTable;
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
