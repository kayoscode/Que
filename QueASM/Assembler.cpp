
#define _CRT_SECURE_NO_WARNINGS

#include "Assembler.h"
#include <stdio.h>
#include <vector>

constexpr int UNDEFINED_CODE = 0x7FFFFFFF;
constexpr int COMMA_CODE = 100;
constexpr int IDENTIFIER_CODE = 101;
constexpr int STRING_CODE = 101;
constexpr int BINARY_INTEGER_CODE = 103;
constexpr int OCTAL_INTER_CODE = 104;
constexpr int HEX_INTEGER_CODE = 105;
constexpr int INTEGER_CODE = 106;
constexpr int FLOAT_CODE = 107;
constexpr int COLON_CODE = 108;
int INT_REGISTER_SERIES_LOW_CODE = 300;
int INT_REGISTER_SERIES_HIGH_CODE = 319;
int FLOAT_REGISTER_SERIES_LOW_CODE = 325;
int FLOAT_REGISTER_SERIES_HIGH_CODE = 339;

int strcicmp(char const *a, char const *b)
{
	for (;; a++, b++) {
		int d = tolower((unsigned char)*a) - tolower((unsigned char)*b);
		if (d != 0 || !*a)
			return d;
	}
}

bool Assembler::ReserveTableCompare::operator()(const std::string& a, const std::string& b) const {
	return strcicmp(a.c_str(), b.c_str()) < 0;
}

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
uint32_t encodeInstruction(uint8_t opcode, bool immediateFlag, uint8_t op1, uint8_t op2, uint8_t op3) {
	uint32_t finalInstruction = opcode;
	finalInstruction |= immediateFlag << 7;
	finalInstruction |= op1 << 8;
	finalInstruction |= op2 << 13;
	finalInstruction |= op3 << 18;

	return finalInstruction;
}

Assembler::Assembler() :
	output(nullptr),
	buffer(nullptr),
	bufferIndex(0),
	bufferSize(0),
	currentLineNumber(0),
    reserveTable(),
	symbolTable(),
	logger("", Level::LEVEL_TRACE),
	endOfParse(false),
	pass(0),
	errorsFound(false),
	currentBinaryOffset(0)
{
	logger.setPrefix("");
	logger.setColorDisabled();
    // Create a reserve table with every keyword:

	// 0-100 series: All instruction keywords
	reserveTable.addReserveWord("NOP", NOP);
	reserveTable.addReserveWord("LSL", LSL);
	reserveTable.addReserveWord("LSR", LSR);
	reserveTable.addReserveWord("ADD", ADD);
	reserveTable.addReserveWord("SUB", SUB);
	reserveTable.addReserveWord("MUL", MUL);
	reserveTable.addReserveWord("DIV", DIV);
	reserveTable.addReserveWord("ULSL", ULSL);
	reserveTable.addReserveWord("ULSR", ULSR);
	reserveTable.addReserveWord("UADD", UADD);
	reserveTable.addReserveWord("USUB", USUB);
	reserveTable.addReserveWord("AND", AND);
	reserveTable.addReserveWord("OR", OR);
	reserveTable.addReserveWord("XOR", XOR);
	reserveTable.addReserveWord("MOV", MOV);
	reserveTable.addReserveWord("CMP", CMP);
	reserveTable.addReserveWord("LA", LA);
	reserveTable.addReserveWord("MOVGE", MOV_GE);
	reserveTable.addReserveWord("MOVG", MOV_G);
	reserveTable.addReserveWord("MOV_LE", MOV_LE);
	reserveTable.addReserveWord("MOVL", MOV_L);
	reserveTable.addReserveWord("MOVE", MOV_E);
	reserveTable.addReserveWord("MOVNE", MOV_NE);
	reserveTable.addReserveWord("MOVC", MOV_C);
	reserveTable.addReserveWord("MOV_ITOF", MOV_ITOF);
	reserveTable.addReserveWord("MOV_FTOI", MOV_FTOI);
	reserveTable.addReserveWord("FADD", FADD);
	reserveTable.addReserveWord("FSUB", FSUB);
	reserveTable.addReserveWord("FMUL", FMUL);
	reserveTable.addReserveWord("FDIV", FDIV);
	reserveTable.addReserveWord("FCMP", FCMP);
	reserveTable.addReserveWord("FMOV", FMOV);
	reserveTable.addReserveWord("FS", FS);
	reserveTable.addReserveWord("FL", FL);
	reserveTable.addReserveWord("SB", SB);
	reserveTable.addReserveWord("SH", SH);
	reserveTable.addReserveWord("SW", SW);
	reserveTable.addReserveWord("LB", LB);
	reserveTable.addReserveWord("LH", LH);
	reserveTable.addReserveWord("LW", LW);
	reserveTable.addReserveWord("JMP", JMP);
	reserveTable.addReserveWord("CALL", CALL);
	reserveTable.addReserveWord("JGE", JGE);
	reserveTable.addReserveWord("JG", JG);
	reserveTable.addReserveWord("JLE", JLE);
	reserveTable.addReserveWord("JL", JL);
	reserveTable.addReserveWord("JE", JE);
	reserveTable.addReserveWord("JNE", JNE);
	reserveTable.addReserveWord("JC", JC);
	reserveTable.addReserveWord("PUSHB", PUSHB);
	reserveTable.addReserveWord("POPB", POPB);
	reserveTable.addReserveWord("SWI", SWI);
	//reserveTable.addReserveWord("HWI", HWI); // HWI is a nop when called from software
	reserveTable.addReserveWord("MOD", MOD);
	reserveTable.addReserveWord("MOV_UITOF", MOV_UITOF);
	reserveTable.addReserveWord("PUSHH", PUSHH);
	reserveTable.addReserveWord("PUSHW", PUSHW);
	reserveTable.addReserveWord("PUSH", PUSHW);
	reserveTable.addReserveWord("POPH", POPH);
	reserveTable.addReserveWord("POPW", POPW);
	reserveTable.addReserveWord("POP", POPW);
	reserveTable.addReserveWord("PUSHF", PUSHF);
	reserveTable.addReserveWord("POPF", POPF);
	reserveTable.addReserveWord("INC", INC);
	reserveTable.addReserveWord("DEC", DEC);
	reserveTable.addReserveWord("RET", RET);

	// 200-300 series: All keywords which are not instructions
	reserveTable.addReserveWord("segment", 200);
	reserveTable.addReserveWord("data", 201);
	reserveTable.addReserveWord("text", 202);
	reserveTable.addReserveWord("byte", 203);
	reserveTable.addReserveWord("half", 204);
	reserveTable.addReserveWord("word", 205);
	reserveTable.addReserveWord("float", 206);
	reserveTable.addReserveWord("ascii", 207);
	reserveTable.addReserveWord("asciiz", 208);
	reserveTable.addReserveWord("space", 209);
	reserveTable.addReserveWord("const", 210);
	reserveTable.addReserveWord("import", 211); // Depricated keyword, instead use label anywhere not in a segment to change it to an import
	reserveTable.addReserveWord("export", 212);
	reserveTable.addReserveWord("end", 213);
	reserveTable.addReserveWord("label", 214);
	// The entry point keyword specifies where the program should start.
	// Only one can be in a single file, and it's exposed in the header
	// for the linker.
	reserveTable.addReserveWord("entrypt", 215);

	// Maybe add this later.
	//reserveTable.addReserveWord("EXPR", 210);

	// 300-325 series: INT registers
	INT_REGISTER_SERIES_LOW_CODE = 300;
	reserveTable.addReserveWord("R0", 300);
	reserveTable.addReserveWord("R1", 301);
	reserveTable.addReserveWord("R2", 302);
	reserveTable.addReserveWord("R3", 303);
	reserveTable.addReserveWord("R4", 304);
	reserveTable.addReserveWord("R5", 305);
	reserveTable.addReserveWord("R6", 306);
	reserveTable.addReserveWord("R7", 307);
	reserveTable.addReserveWord("R8", 308);
	reserveTable.addReserveWord("R9", 309);
	reserveTable.addReserveWord("R10", 310);
	reserveTable.addReserveWord("R11", 311);
	reserveTable.addReserveWord("R12", 312);
	reserveTable.addReserveWord("R13", 313);
	reserveTable.addReserveWord("R14", 314);
	reserveTable.addReserveWord("R15", 315);
	reserveTable.addReserveWord("PC", 316);
	reserveTable.addReserveWord("BP", 317);
	reserveTable.addReserveWord("SP", 318);
	reserveTable.addReserveWord("FR", 319);
	reserveTable.addReserveWord("RA", 320);
	INT_REGISTER_SERIES_HIGH_CODE = 320;

	// 325 - 350 series: FLOAT REGISTERS
	FLOAT_REGISTER_SERIES_LOW_CODE = 325;
	reserveTable.addReserveWord("F0", 325);
	reserveTable.addReserveWord("F1", 326);
	reserveTable.addReserveWord("F2", 327);
	reserveTable.addReserveWord("F3", 328);
	reserveTable.addReserveWord("F4", 329);
	reserveTable.addReserveWord("F5", 330);
	reserveTable.addReserveWord("F6", 331);
	reserveTable.addReserveWord("F7", 332);
	reserveTable.addReserveWord("F8", 333);
	reserveTable.addReserveWord("F9", 334);
	reserveTable.addReserveWord("F10", 335);
	reserveTable.addReserveWord("F11", 336);
	reserveTable.addReserveWord("F12", 337);
	reserveTable.addReserveWord("F13", 338);
	reserveTable.addReserveWord("F14", 339);
	reserveTable.addReserveWord("F15", 340);
	FLOAT_REGISTER_SERIES_HIGH_CODE = 340;
}

bool Assembler::assembleFile(const std::string& fileName, std::ostream& outputFile) {
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
		symbolTable.clear();

		collectNextToken();

		// Parse the assembly.
		currentBinaryOffset = 0;
		errorsFound = false;
		pass = 0;
		parseFile();

		int outputBufferSize = currentBinaryOffset;
		currentBinaryOffset = 0;
		currentLineNumber = 0;
		bufferIndex = 0;
		endOfParse = false;

		if (!errorsFound) {
			collectNextToken();
			pass = 1;
			output = new char[outputBufferSize];
			parseFile();

			// Collect header info and write to the file.
			writeOBJHeader(outputFile);
			// Write the size of the binary to the output stream
			outputFile.write((char*)&outputBufferSize, sizeof(uint32_t));
			outputFile.write(output, outputBufferSize);

			// Write binary information on the first word boundary after the header.
			delete[] output;
		}

		delete[] buffer;
	}
	else {
		logger.error("File could not be loaded from disk.");
	}

	return !errorsFound;
}

void Assembler::writeOBJHeader(std::ostream& output) {
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
	output.write((char*)&symbolTable.exportedSymbolCount, sizeof(uint32_t));

	for (std::map<std::string, SymbolType>::iterator i = symbolTable.symbolTypes.begin(); i != symbolTable.symbolTypes.end(); ++i) {
		if (i->second == SymbolType::EXPORTED) {
			output.write(i->first.c_str(), i->first.size() + 1);
			int value = symbolTable.getSymbolValue(i->first);
			output.write((char*)&value, sizeof(uint32_t));
		}
	}

	// Write imported symbols to file.
	output.write((char*)&symbolTable.importedSymbolCount, sizeof(uint32_t));
	for (std::map<std::string, SymbolType>::iterator i = symbolTable.symbolTypes.begin(); i != symbolTable.symbolTypes.end(); ++i) {
		if (i->second == SymbolType::IMPORTED) {
			std::vector<int>& references = symbolTable.symbolImportReferences[i->first];
			int referenceCount = references.size();

			if (referenceCount <= 0) {
				// logger.warning("Unused imported symbol: {s}", i->first.c_str());
			}

			output.write(i->first.c_str(), i->first.size() + 1);

			// Write N occurrences
			output.write((char*)&referenceCount, sizeof(uint32_t));

			// Write offset to each occurence
			for (int i = 0; i < references.size(); i++) {
				output.write((char*)&references[i], sizeof(uint32_t));
			}
		}
	}
}

/// <summary>
/// Returns whether a character counts as white space for this parser
/// </summary>
/// <param name="character"></param>
/// <returns></returns>
static bool isWhiteSpace(char character) {
	return character == ' ' || character == '\n' || character == '\r' || character == '\t';
}

static bool isAlpha(char character) {
	return (character >= 'A' && character <= 'Z') || (character >= 'a' && character <= 'z');
}

static bool isNum(char character) {
	return character >= '0' && character <= '9';
}

void Assembler::trimWhiteSpace() {
	// Enter into a comment till the end of the line
	if (buffer[bufferIndex] == ';') {
		while (!isEndOfStream() && buffer[bufferIndex] != '\n') {
			bufferIndex++;
		}
	}

	while (!isEndOfStream() && isWhiteSpace(buffer[bufferIndex])) {
		if (buffer[bufferIndex] == '\n') {
			currentLineNumber++;
		}

		bufferIndex++;
	}

	if (buffer[bufferIndex] == ';') {
		trimWhiteSpace();
	}
}

void Assembler::readIdentifierOrKeyword() {
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

void Assembler::readNumber() {
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

bool Assembler::readOneChar() {
	if (buffer[bufferIndex] == ',') {
		currentToken.lexeme = ",";
		currentToken.code = COMMA_CODE;
		return true;
	}
	else if (buffer[bufferIndex] == ':') {
		currentToken.lexeme = ":";
		currentToken.code = COLON_CODE;
		return true;
	}

	return false;
}

void Assembler::readString() {
	bufferIndex++;
	char* lexemeStart = buffer + bufferIndex;

	while (!isEndOfStream() && buffer[bufferIndex] != '"' && buffer[bufferIndex] != '\r' && buffer[bufferIndex] != '\n') {
		bufferIndex++;
	}

	currentToken.lexeme = std::string(lexemeStart, buffer + bufferIndex);
	currentToken.code = STRING_CODE;

	if (buffer[bufferIndex] != '"') {
		addWarning("String ran off the end of the line without closing quote, may cause unexpected behavior");
	}

	bufferIndex++;
}

bool Assembler::collectNextToken() {
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
		else if (readOneChar()) {
			bufferIndex++;
		}
		else {
			// An error occurs in this case
			endOfParse = true;
			currentToken.lexeme = buffer[bufferIndex];
			currentToken.code = UNDEFINED_CODE;
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
/// At the start of the assembly file, there can be one of two tokens.
/// All imported functions must appear outside of any segment and there can be infinite
/// ASMFILE => import [idt], ... | segment SEGMENT
/// SEGMENT => TEXT_SEGMENT | DATA_SEGMENT
/// TEXT_SEGMENT => .text INSTRUCTION_LIST
/// DATA_SEGMENT => .data DATA_LIST
/// INSTRUCTION_LIST => INSTRUCTION | export INSTRUCTION
/// INSTRUCTION => ...
/// DATA_LIST => DATA_TYPE VALUE
/// 
/// The export keyword ensures that the instruction is visible in the global scope of the assembly.
/// Import lets the assembler know to not treat an unrecogized identifier as an error as it's imported
/// from the global scope.
/// </summary>
void Assembler::parseFile() {
	// A file object can contain an infinite number of segments and an infinite number of imports.
	// The imports must be at the top of the file.

	// Gather all segments.
	while (!isEndOfStream()) {
		if (currentToken.code == reserveTable.getReserveCode("segment")) {
			collectNextToken();
			parseSegment();
		}
		else if (currentToken.code == reserveTable.getReserveCode("label")) {
			collectNextToken();
			parseImport();
		}
		else if (currentToken.code == reserveTable.getReserveCode("const")) {
			collectNextToken();
			parseConst();
		}
		else {
			addError("Unexpected token in file");
			collectNextToken();
			break;
		}
	}
}

/// <summary>
/// Imports can be considered anything which 
/// </summary>
void Assembler::parseImport() {
	if (currentToken.code == IDENTIFIER_CODE) {
		if(pass == 0)
		if (!symbolTable.addSymbol(currentToken.lexeme, SymbolType::IMPORTED, 0)) {
			addError("Previously declared symbol");
		}
	}
	else {
		addError("Expected identifer after import");
	}

	collectNextToken();
}

void Assembler::parseSegment() {
	if (currentToken.code == reserveTable.getReserveCode("data")) {
		collectNextToken();
		parseDataSegment();
	}
	else if (currentToken.code == reserveTable.getReserveCode("text")) {
		collectNextToken();
		parseTextSegment();
	}
	else {
		addError("Exepcted segment type after segment declaration");
		collectNextToken();
	}
}

/// <summary>
/// Only one string may be written to an ascii data section.
/// </summary>
/// <param name="nullTerminated"></param>
void Assembler::parseAscii(bool nullTerminated) {
	if (currentToken.code == STRING_CODE) {
		std::string str = currentToken.lexeme;
		// if null terminated, write in a zero.

		if (pass == 1) {
			// Write string to file.
			for (int i = 0; i < str.size(); i++) {
				output[currentBinaryOffset + i] = str[i];
			}

			if (nullTerminated) {
				output[currentBinaryOffset + str.size()] = 0;
			}
		}

		currentBinaryOffset += str.size() + nullTerminated;
	}
	else {
		addError("Expected string");
	}

	collectNextToken();
}

/// <summary>
/// The data segment can include a list of data specifiers
/// </summary>
void Assembler::parseDataSegment() {
	while (true) {
		if (currentToken.code == reserveTable.getReserveCode("byte")) {
			collectNextToken();
			parseByte();
			// Align to word boundary
			currentBinaryOffset = (currentBinaryOffset + 3) & 0xFFFFFFFC;
		}
		else if (currentToken.code == reserveTable.getReserveCode("half")) {
			collectNextToken();
			parseHalf();
			// Align to word boundary
			currentBinaryOffset = (currentBinaryOffset + 3) & 0xFFFFFFFC;
		}
		else if (currentToken.code == reserveTable.getReserveCode("word")) {
			collectNextToken();
			parseWord();
		}
		else if (currentToken.code == reserveTable.getReserveCode("float")) {
			collectNextToken();
			parseFloat();
		}
		else if (currentToken.code == reserveTable.getReserveCode("ascii")) {
			collectNextToken();
			parseAscii(false);
			// Align to word boundary
			currentBinaryOffset = (currentBinaryOffset + 3) & 0xFFFFFFFC;
		}
		else if (currentToken.code == reserveTable.getReserveCode("asciiz")) {
			collectNextToken();
			parseAscii(true);
			// Align to word boundary
			currentBinaryOffset = (currentBinaryOffset + 3) & 0xFFFFFFFC;
		}
		else if (currentToken.code == reserveTable.getReserveCode("space")) {
			collectNextToken();
			parseSpace();
			// Align to word boundary
			currentBinaryOffset = (currentBinaryOffset + 3) & 0xFFFFFFFC;
		}
		else if (currentToken.code == reserveTable.getReserveCode("label")) {
			collectNextToken();
			parseLabel();
		}
		else if (currentToken.code == reserveTable.getReserveCode("export")) {
			collectNextToken();
			parseExport();
		}
		else if (currentToken.code == reserveTable.getReserveCode("end")) {
			collectNextToken();
			break;
		}
		else {
			addError("Invalid token within data segment");
			collectNextToken();
			break;
		}
	}
}

/// <summary>
/// Written into the space data segment, one number representing the size of the buffer
/// may be specified.
/// </summary>
void Assembler::parseSpace() {
	int value = 0;

	if (getIntegerFromToken(value, false)) {
	}
	else {
		addError("Expected positive numeric value or identifier");
	}

	if (value < 0) {
		addError("Invalid operation: negative space");
	}
	else if (value == 0) {
		addWarning("Zero space added to space data segment");
	}
	else {
		currentBinaryOffset += value;
	}

	collectNextToken();
}

bool Assembler::getIntegerFromToken(int& value, bool acceptLabels) {
	int64_t parseValue = 0;
	value = 0;

	bool negativeFlag = false;
	if (currentToken.lexeme.size() > 0) {
		negativeFlag = currentToken.lexeme[0] == '-';
	}

	try {
		if (currentToken.code == INTEGER_CODE) {
			parseValue = std::stoll(currentToken.lexeme, 0, 10);
		}
		else if (currentToken.code == HEX_INTEGER_CODE) {
			std::string symbol = currentToken.lexeme.substr(2 + (uint64_t)negativeFlag);
			parseValue = std::stoll(symbol, 0, 16) * (negativeFlag? -1 : 1);
		}
		else if (currentToken.code == OCTAL_INTER_CODE) {
			std::string symbol = currentToken.lexeme.substr(2 + (uint64_t)negativeFlag);
			parseValue = std::stoll(symbol, 0, 8) * (negativeFlag? -1 : 1);
		}
		else if (currentToken.code == BINARY_INTEGER_CODE) {
			std::string symbol = currentToken.lexeme.substr(2 + (uint64_t)negativeFlag);
			parseValue = std::stoll(symbol, 0, 2) * (negativeFlag? -1 : 1);
		}
		else if (currentToken.code == IDENTIFIER_CODE) {
			SymbolType type = symbolTable.getSymbolType(currentToken.lexeme);
			if (type == SymbolType::CONSTANT) {
				parseValue = symbolTable.getSymbolValue(currentToken.lexeme);
			}
			else if (type == LOCAL || type == EXPORTED) {
				if (acceptLabels) {
					parseValue = (int64_t)(symbolTable.getSymbolValue(currentToken.lexeme) - (currentBinaryOffset + 4ULL));
				}
				else {
					addError("Labels not accepted");
					return false;
				}
			}
			else if (type == IMPORTED) {
				if (acceptLabels) {
					// These are external addresses and should be resolved by the linker.
					parseValue = 0;

					if (pass == 0) {
						symbolTable.addImportReference(currentToken.lexeme, currentBinaryOffset + 4);
					}
				}
				else {
					addError("Labels not accepted");
					return false;
				}
			}
			else {
				// If it's the first pass, we don't yet know if the identifier will be declared later.
				if (pass == 0) {
					return true;
				}
				else {
					return false;
				}
			}
		}
		else {
			return false;
		}
	}
	catch (const std::exception&) {
		return false;
	}

	value = (uint32_t)parseValue;
	return true;
}

bool Assembler::getFloatFromToken(float& value) {
	long double dvalue = 0;

	if (currentToken.code == FLOAT_CODE) {
		dvalue = std::stold(currentToken.lexeme, 0);
	}
	// Integers may also be parsed in as floats if the programmer desires.
	else if (currentToken.code == INTEGER_CODE) {
		dvalue = std::stold(currentToken.lexeme, 0);
	}
	// Load from identifier if possible
	else if (symbolTable.getSymbolType(currentToken.lexeme) == SymbolType::FLOAT_CONSTANT) {
		uint32_t value = symbolTable.getSymbolValue(currentToken.lexeme);
		float newValue = *(float*)&value;
		dvalue = newValue;
	}
	else {
		return false;
	}

	value = (float)dvalue;

	return true;
}

/// <summary>
/// A constant should have a unique identifier followed by 
/// a single numerical value.
/// </summary>
void Assembler::parseConst() {
	std::string idt = "";

	if (currentToken.code == IDENTIFIER_CODE) {
		idt = currentToken.lexeme;
		collectNextToken();
		int intValue = 0;
		float floatValue = 0;

		if (getIntegerFromToken(intValue, false)) {
			if(pass == 0)
			if (!symbolTable.addSymbol(idt, SymbolType::CONSTANT, intValue)) {
				addError("Previously declared symbol");
			}
		}
		else if (getFloatFromToken(floatValue)) {
			if(pass == 0)
			if (!symbolTable.addSymbol(idt, SymbolType::FLOAT_CONSTANT, *(int*)&floatValue)) {
				addError("Previously declared symbol");
			}
		}
		else {
			addError("Expected a numeric value or valid identifier");
		}

		collectNextToken();
	}
	else {
		addError("Expected identifier");
	}
}

/// <summary>
/// Should be a comma delimiated list of values between 0 and 255.
/// </summary>
void Assembler::parseByte() {
	int value = 0;
	uint8_t v = 0;

	if (getIntegerFromToken(value, false)) {
	}
	else {
		addError("Expected numeric value or valid identifier");
	}

	if (value <= UCHAR_MAX && value >= CHAR_MIN) {
		v = value & 0xFF;
	}
	else{
		addWarning("Value overflows 1 byte range, may cause unexpected results");
	}

	if (pass == 1) {
		output[currentBinaryOffset] = v;
	}

	currentBinaryOffset += sizeof(uint8_t);

	collectNextToken();

	if (currentToken.code == COMMA_CODE) {
		collectNextToken();
		parseByte();
	}
}

void Assembler::parseHalf() {
	int value = 0;
	uint16_t v = 0;

	if (getIntegerFromToken(value, false)) {
	}
	else {
		addError("Expected numeric value or valid identifier");
	}

	if (value <= UINT16_MAX && value >= INT16_MIN) {
		v = value & 0xFFFF;
	}
	else{
		addWarning("Value overflows 2 byte range, may cause unexpected results");
	}

	if (pass == 1) {
		uint16_t* outputBuf = (uint16_t*)output;
		outputBuf[currentBinaryOffset / sizeof(uint16_t)] = v;
	}

	currentBinaryOffset += sizeof(uint16_t);

	collectNextToken();

	if (currentToken.code == COMMA_CODE) {
		collectNextToken();
		parseHalf();
	}
}

void Assembler::parseWord() {
	int value = 0;
	if (getIntegerFromToken(value, false)) {
	}
	else {
		addError("Expected numeric value or valid identifier");
	}

	collectNextToken();

	if (pass == 1) {
		uint32_t* outputBuf = (uint32_t*)output;
		outputBuf[currentBinaryOffset / sizeof(uint32_t)] = value;
	}

	currentBinaryOffset += sizeof(uint32_t);

	if (currentToken.code == COMMA_CODE) {
		collectNextToken();
		parseWord();
	}
}

void Assembler::parseFloat() {
	float value = 0;
	if (getFloatFromToken(value)) {
	}
	else {
		addError("Expected floating point value");
	}

	if (pass == 1) {
		float* outputBuf = (float*)output;
		outputBuf[currentBinaryOffset / sizeof(float)] = value;
	}

	currentBinaryOffset += sizeof(float);

	collectNextToken();

	if (currentToken.code == COMMA_CODE) {
		collectNextToken();
		parseFloat();
	}
}

void Assembler::parseNoArgs(int opcode) {
	// Write instruction to file.
	uint32_t instruction = 0;
	if (opcode == NOP) {
		instruction = encodeInstruction(0, 0, 0, 0, 0);
	}
	else if (opcode == RET) {
		// JMP RA
		instruction = encodeInstruction(JMP, 0, 20, 0, 0);
	}
	
	writeInstruction(instruction, false, 0);
}

bool Assembler::readIntRegister(int& registerIndex) {
	if (currentToken.code >= INT_REGISTER_SERIES_LOW_CODE && currentToken.code <= INT_REGISTER_SERIES_HIGH_CODE) {
		registerIndex = currentToken.code - INT_REGISTER_SERIES_LOW_CODE;
		return true;
	}

	return false;
}

bool Assembler::readFloatRegister(int& registerIndex) {
	if (currentToken.code >= FLOAT_REGISTER_SERIES_LOW_CODE && currentToken.code <= FLOAT_REGISTER_SERIES_HIGH_CODE) {
		registerIndex = currentToken.code - FLOAT_REGISTER_SERIES_LOW_CODE;
		return true;
	}

	return false;
}

void Assembler::parseThreeArgsFloat(int opcode) {
	// The first and second args must be int registers, but the last arg can be an immediate or idt.
	int arg1, arg2, arg3;
	float imm = 0;
	bool immediateFlag = false;
	bool success = true;

	if (readFloatRegister(arg1)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (readFloatRegister(arg2)) {
			collectNextToken();

			if (currentToken.code == COMMA_CODE) {
				collectNextToken();
			}

			if (!readFloatRegister(arg3)) {
				// It can also be an idt/imm
				if (getFloatFromToken(imm)) {
					immediateFlag = true;
					collectNextToken();
				}
				else {
					collectNextToken();
					addError("Expected regf32/imm/idt");
					success = false;
				}
			}
			else {
				collectNextToken();
			}
		}
		else {
			goto parseThreeArgsFloatErr;
		}
	}
	else {
	parseThreeArgsFloatErr:
		addError("Expected regf32");
		success = false;
		collectNextToken();
	}

	uint32_t instruction = 0;
	if (success) {
		// Write instruction to file.
		if (!immediateFlag) {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, arg2, arg3);
		}
		else {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, arg2, 0);
		}

		uint32_t immediate = *(uint32_t*)&imm;
		writeInstruction(instruction, immediateFlag, immediate);
	}
}

void Assembler::parseThreeArgsInt(int opcode) {
	// The first and second args must be int registers, but the last arg can be an immediate or idt.
	int arg1, arg2, arg3;
	bool immediateFlag = false;
	bool success = true;

	if (readIntRegister(arg1)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (readIntRegister(arg2)) {
			collectNextToken();

			if (currentToken.code == COMMA_CODE) {
				collectNextToken();
			}

			if (!readIntRegister(arg3)) {
				// It can also be an idt/imm
				if (getIntegerFromToken(arg3, false)) {
					immediateFlag = true;
				}
				else {
					addError("Expected reg32/imm/idt");
					success = false;
				}
			}
		}
		else {
			goto parseThreeArgsIntErr;
		}
	}
	else {
	parseThreeArgsIntErr:
		addError("Expected reg32");
		success = false;
	}

	uint32_t instruction = 0;
	if (success) {
		// Write instruction to file.
		if (!immediateFlag) {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, arg2, arg3);
		}
		else {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, arg2, 0);
		}

		writeInstruction(instruction, immediateFlag, arg3);
	}

	collectNextToken();
}

void Assembler::parseTwoArgsFloat(int opcode) {
	int arg1, arg2;
	float imm = 0;
	bool immediateFlag = false;
	bool success = true;

	if (readFloatRegister(arg1)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (!readFloatRegister(arg2)) {
			if (getFloatFromToken(imm)) {
				immediateFlag = true;
			}
			else {
				addError("Expected regf32/imm/idt");
				success = false;
			}
		}
	}
	else {
		addError("Expected regf32");
		success = false;
	}

	uint32_t instruction = 0;
	if (success) {
		// Write instruction to file.
		if (!immediateFlag) {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, arg2, 0);
		}
		else {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, 0, 0);
		}

		writeInstruction(instruction, immediateFlag, *(uint32_t*)&imm);
	}

	collectNextToken();
}

void Assembler::parseTwoArgsInt(int opcode) {
	int arg1, arg2;
	bool immediateFlag = false;
	bool success = true;

	if (readIntRegister(arg1)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (!readIntRegister(arg2)) {
			if (getIntegerFromToken(arg2, (opcode == LA))) {
				immediateFlag = true;
			}
			else {
				addError("Expected reg32/imm/idt");
				success = false;
			}
		}
	}
	else {
		addError("Expected reg32");
		success = false;
	}

	uint32_t instruction = 0;
	if (success) {
		// Write instruction to file.
		if (!immediateFlag) {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, arg2, 0);
		}
		else {
			instruction = encodeInstruction(opcode, immediateFlag, arg1, 0, 0);
		}

		writeInstruction(instruction, immediateFlag, arg2);
	}

	collectNextToken();
}

void Assembler::parseIntToFloat(int opcode) {
	int arg1, arg2;
	bool success = true;

	if (readFloatRegister(arg1)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (!readIntRegister(arg2)) {
			addError("Expected reg32");
			success = false;
		}
	}
	else {
		addError("Expected regf32");
		success = false;
	}

	uint32_t instruction = 0;
	if (success) {
		instruction = encodeInstruction(opcode, false, arg1, arg2, 0);

		writeInstruction(instruction, false, 0);
	}

	collectNextToken();
}

void Assembler::parseFloatToInt(int opcode) {
	int arg1, arg2;
	bool success = true;

	if (readIntRegister(arg1)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (!readFloatRegister(arg2)) {
			addError("Expected regf32");
			success = false;
		}
	}
	else {
		addError("Expected reg32");
		success = false;
	}

	uint32_t instruction = 0;
	if (success) {
		instruction = encodeInstruction(opcode, false, arg1, arg2, 0);
		writeInstruction(instruction, false, 0);
	}

	collectNextToken();
}

void Assembler::parseStoreLoad(int opcode) {
	int source, addr;
	bool immediateFlag = 0;
	int offset = 0;
	bool flt = false;
	bool success = true;

	if (opcode == FL || opcode == FS) {
		if (readFloatRegister(source)) {
			collectNextToken();
			flt = true;
		}
		else {
			addError("Expected regf32");
			success = false;
		}
	}
	else {
		if (readIntRegister(source)) {
			collectNextToken();
		}
		else {
			addError("Expected reg32");
			success = false;
		}
	}

	if (currentToken.code == COMMA_CODE) {
		collectNextToken();
	}

	if (readIntRegister(addr)) {
		collectNextToken();

		if (currentToken.code == COMMA_CODE) {
			collectNextToken();
		}

		if (getIntegerFromToken(offset, false)) {
			collectNextToken();
			immediateFlag = true;
		}
	}
	else {
		addError("Expected address reg32");
		success = false;
		collectNextToken();
	}

	uint32_t instruction = 0;
	if (success) {
		if (!immediateFlag) {
			instruction = encodeInstruction(opcode, immediateFlag, source, addr, 0);
		}
		else {
			instruction = encodeInstruction(opcode, immediateFlag, source, addr, offset);
		}

		writeInstruction(instruction, immediateFlag, offset);
	}
}

void Assembler::parseJump(int opcode) {
	int addr;
	bool immediate = false;
	uint32_t instruction = 0;

	if (readIntRegister(addr)) {
		// Immediate jump.
		collectNextToken();
		instruction = encodeInstruction(opcode, false, addr, 0, 0);
		writeInstruction(instruction, false, 0);
	}
	else if (getIntegerFromToken(addr, true)) {
		// Register jump.
		collectNextToken();
		instruction = encodeInstruction(opcode, true, 0, 0, 0);
		writeInstruction(instruction, true, addr);
	}
	else {
		addError("Expected reg32/imm/idt");
		collectNextToken();
	}
}

void Assembler::parsePushPop(int opcode) {
	int reg = 0;

	if (opcode == PUSHF || opcode == POPF) {
		if (!readFloatRegister(reg)) {
			addError("Expected regf32");
		}
	}
	else {
		if (!readIntRegister(reg)) {
			addError("Expected reg32");
		}
	}

	collectNextToken();

	uint32_t instruction = encodeInstruction(opcode, false, reg, 0, 0);
	writeInstruction(instruction, false, 0);
}

void Assembler::parseIncDec(int opcode) {
	int reg;
	uint32_t instruction = 0;
	if (readIntRegister(reg)) {
		instruction = encodeInstruction(opcode, 0, reg, 0, 0);
		writeInstruction(instruction, false, 0);
	}
	else {
		addError("Expected reg32");
	}

	collectNextToken();
}

void Assembler::parseSwi(int opcode) {
	int immediate = 0;

	uint32_t instruction = 0;
	if (getIntegerFromToken(immediate, false)) {
		// All of these instructions are guaranteed to have an immediate, no need to encode it.
		instruction = encodeInstruction(opcode, false, 0, 0, 0);
		writeInstruction(instruction, true, immediate);
	}
	else {
		addError("Expected imm/idt");
	}

	collectNextToken();
}

/// <summary>
/// Instructions are sorted and handled in bulk based upon which ones share common arguments.
/// </summary>
void Assembler::parseTextSegment() {
	while (true) {
		// Instructions with no arguments (type 1).
		if (currentToken.code == reserveTable.getReserveCode("entrypt")) {
			if (pass == 0) {
				if (entryPoint == -1) {
					entryPoint = currentBinaryOffset;
				}
				else {
					addError("Only one entrypoint may be defined");
				}
			}

			collectNextToken();
		}
		else if (currentToken.code == NOP || currentToken.code == RET) {
			int opcode = currentToken.code;
			collectNextToken();
			parseNoArgs(opcode);
		}
		// Instructions with three arguments with a potential int immediate (type 2).
		else if (currentToken.code >= LSL && currentToken.code <= XOR || currentToken.code == MOD) {
			int opcode = currentToken.code;
			collectNextToken();
			parseThreeArgsInt(opcode);
		}
		// Two arguments and one potential immediate ints.
		else if (currentToken.code >= MOV && currentToken.code <= MOV_C) {
			int opcode = currentToken.code;
			collectNextToken();
			parseTwoArgsInt(opcode);
		}
		// Move from int register to float register.
		else if (currentToken.code == MOV_ITOF || currentToken.code == MOV_UITOF) {
			int opcode = currentToken.code;
			collectNextToken();
			parseIntToFloat(opcode);
		}
		// Move from float register to int register.
		else if (currentToken.code == MOV_FTOI) {
			collectNextToken();
			parseFloatToInt(MOV_FTOI);
		}
		// Floating point 
		else if (currentToken.code >= FADD && currentToken.code <= FDIV) {
			int opcode = currentToken.code;
			collectNextToken();
			parseThreeArgsFloat(opcode);
		}
		else if (currentToken.code == FCMP || currentToken.code == FMOV) {
			int opcode = currentToken.code;
			collectNextToken();
			parseTwoArgsFloat(opcode);
		}
		else if (currentToken.code >= FS && currentToken.code <= LW) {
			int opcode = currentToken.code;
			collectNextToken();
			parseStoreLoad(opcode);
		}
		else if (currentToken.code >= JMP && currentToken.code <= JC) {
			int opcode = currentToken.code;
			collectNextToken();
			parseJump(opcode);
		}
		else if (currentToken.code == PUSHB || currentToken.code == POPB ||
			(currentToken.code >= PUSHH && currentToken.code <= POPF)) {
			int opcode = currentToken.code;
			collectNextToken();
			parsePushPop(opcode);
		}
		else if (currentToken.code == INC || currentToken.code == DEC) {
			int opcode = currentToken.code;
			collectNextToken();
			parseIncDec(opcode);
		}
		else if (currentToken.code == SWI) {
			int opcode = currentToken.code;
			collectNextToken();
			parseSwi(opcode);
		}
		else if (currentToken.code == reserveTable.getReserveCode("label")) {
			collectNextToken();
			parseLabel();
		}
		else if (currentToken.code == reserveTable.getReserveCode("export")) {
			collectNextToken();
			parseExport();
		}
		else if (currentToken.code == reserveTable.getReserveCode("end")) {
			collectNextToken();
			break;
		}
		else {
			addError("Invalid token within text segment");
			collectNextToken();
			break;
		}
	}
}

void Assembler::parseExport() {
	if (currentToken.code == IDENTIFIER_CODE) {
		if (pass == 0) {
			if (!symbolTable.addSymbol(currentToken.lexeme, SymbolType::EXPORTED, currentBinaryOffset)) {
				addError("Previously declared symbol");
			}
		}
		collectNextToken();

		if (currentToken.code == COLON_CODE) {
		}
		else {
			addError("Expected a colon after external label declaration");
		}
	}
	else {
		addError("Expected identifier");
	}

	collectNextToken();
}

/// <summary>
/// Keyword label followed by a new identifier followed by a colon.
/// TODO: calculate the label value from its offset in the file.
/// </summary>
void Assembler::parseLabel() {
	if (currentToken.code == IDENTIFIER_CODE) {
		if (pass == 0) {
			if (!symbolTable.addSymbol(currentToken.lexeme, SymbolType::LOCAL, currentBinaryOffset)) {
				addError("Previously declared symbol");
			}
		}

		collectNextToken();

		if (currentToken.code == COLON_CODE) {
		}
		else {
			addError("Expected a colon after label declaration");
		}
	}
	else {
		addError("Expected identifier");
	}

	collectNextToken();
}