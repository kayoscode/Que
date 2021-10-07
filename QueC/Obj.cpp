#define _CRT_SECURE_NO_WARNINGS

#include "Compiler.h"

Compiler::~Compiler()
{
}

bool Compiler::compileFile(const std::string& fileName, std::ostream& outputStream) {
	logger.setColorDisabled();
	logger.setPrefix("");
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

		currentBinaryOffset = 0;
		errorsFound = false;
		pass = 0;

		collectNextToken();
		parseFile();

		currentBinaryOffset = 0;
		currentLineNumber = 0;
		bufferIndex = 0;
		endOfParse = false;

		// If this happens, theres a bug.
		if (!symbolStack.atGlobalScope()) {
			addError("It appears a stack frame was not closed. This is a bug in the compiler.");
			errorsFound = true;
		}

		if (!errorsFound) {
			collectNextToken();
			pass = 1;
			parseFile();

			int outputBufferSize = currentBinaryOffset;
			currentBinaryOffset = 0;
			currentLineNumber = 0;
			bufferIndex = 0;
			endOfParse = false;
			pass = 2;

			if (!errorsFound) {
				collectNextToken();
				output = new char[outputBufferSize];
				parseFile();

				if (!errorsFound) {
					// WriteOBJ header
					writeOBJHeader(outputStream);
					// Write bin to the file.
					outputStream.write((char*)&outputBufferSize, sizeof(uint32_t));
					outputStream.write(output, outputBufferSize);
				}

				delete[] output;
			}
		}

		delete[] buffer;
	}
	else {
		logger.error("File could not be loaded from disk.");
		errorsFound = true;
	}

	return !errorsFound;
}


void Compiler::writeOBJHeader(std::ostream& output) {
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
	output.write((char*)&symbolStack.globalSymbols.exportedSymbolCount, sizeof(uint32_t));

	for (std::map<std::string, SymbolInfo*>::iterator i = symbolStack.globalSymbols.symbolInfo.begin(); 
		i != symbolStack.globalSymbols.symbolInfo.end(); ++i) {
		if (i->second->accessType == QueSymbolAccessType::EXPORTED) {
			output.write(i->first.c_str(), i->first.size() + 1);
			int value = i->second->value;
			output.write((char*)&value, sizeof(uint32_t));
		}
	}

	// Write imported symbols to file for the text segment.
	uint32_t textSegmentSymbolCount = symbolStack.globalSymbols.symbolImportReferencesTextSeg.size();
	output.write((char*)&textSegmentSymbolCount, sizeof(uint32_t));
	for (std::map<std::string, std::vector<int>>::iterator i = 
		symbolStack.globalSymbols.symbolImportReferencesTextSeg.begin(); 
		i != symbolStack.globalSymbols.symbolImportReferencesTextSeg.end(); ++i) 
	{
		std::vector<int>& references = i->second;
		int referenceCount = references.size();

		output.write(i->first.c_str(), i->first.size() + 1);

		// Write N occurrences
		output.write((char*)&referenceCount, sizeof(uint32_t));

		// Write offset to each occurence
		for (int i = 0; i < references.size(); i++) {
			output.write((char*)&references[i], sizeof(uint32_t));
		}
	}

	uint32_t dataSegmentSymbolCount = symbolStack.globalSymbols.symbolImportReferencesDataSeg.size();
	output.write((char*)&dataSegmentSymbolCount, sizeof(uint32_t));
	// Write the imported symbols for the data segment. (zeros will be fine because the correct value will be a direct substituation)
	for (std::map<std::string, std::vector<int>>::iterator i = 
		symbolStack.globalSymbols.symbolImportReferencesDataSeg.begin(); 
		i != symbolStack.globalSymbols.symbolImportReferencesDataSeg.end(); ++i) 
	{
		std::vector<int>& references = i->second;
		int referenceCount = references.size();

		output.write(i->first.c_str(), i->first.size() + 1);

		// Write N occurrences
		output.write((char*)&referenceCount, sizeof(uint32_t));

		// Write offset to each occurence
		for (int i = 0; i < references.size(); i++) {
			output.write((char*)&references[i], sizeof(uint32_t));
		}
	}

	// Write global offset table.
	uint32_t gotSize = symbolStack.globalSymbols.globalOffsetTable.size();
	output.write((char*)&gotSize, sizeof(uint32_t));
	// Write each occurrence to the file.
	for (int i = 0; i < gotSize; i++) {
		int location = symbolStack.globalSymbols.globalOffsetTable[i];
		output.write((char*)&location, sizeof(uint32_t));
	}
}