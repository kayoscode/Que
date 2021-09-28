#include "Linker.h"
#include <fstream>
#include <stdint.h>

Linker::Linker() 
	:logger("", Level::LEVEL_TRACE)
{
	logger.setPrefix("");
	logger.setColorDisabled();
}

Linker::~Linker() {

}

bool Linker::linkFiles(const std::string& outputFileName, const std::vector<std::string>& objFiles) {
	std::vector<OBJInfo*> objFileInfo;
	offsets.clear();
	globalSymbols.clear();
	int previousOffset = 0;
	hasErrors = false;
	entryPoint = -1;

	// Load obj information into vector.
	for (int i = 0; i < objFiles.size(); i++) {
		offsets.push_back(previousOffset);
		objFileInfo.push_back(new OBJInfo());
		parseOBJ(objFiles[i], *objFileInfo[i]);
		previousOffset += objFileInfo[i]->binarySize;
	}

	// Make sure we have an entrypoint
	if (entryPoint == -1) {
		addError();
		logger.error("No entrypoint defined");
	}

	if (!hasErrors) {
		// Write binary to file adjusting offsets.
		// Ensure each imported symbol is resolved.
		for (int i = 0; i < objFileInfo.size(); i++) {
			resolveSymbols(*objFileInfo[i], offsets[i]);
		}
	}

	if (!hasErrors) {
		std::ofstream outputFile(outputFileName, std::ios::binary | std::ios::out);
		// Write jump into entrypoint.
		int instruction = 0b10101000;
		outputFile.write((char*)&instruction, sizeof(uint32_t));
		entryPoint += 4;
		outputFile.write((char*)&entryPoint, sizeof(uint32_t));

		for (int i = 0; i < objFileInfo.size(); i++) {
			outputFile.write(objFileInfo[i]->binary, objFileInfo[i]->binarySize);
		}

		outputFile.close();
	}

	for (int i = 0; i < objFileInfo.size(); i++) {
		delete objFileInfo[i];
	}

	return !hasErrors;
}

void Linker::resolveSymbols(OBJInfo& objectInfo, int fileOffset) {
	for (std::map<std::string, std::vector<int>>::iterator i = 
		objectInfo.importedSymbolOffsets.begin(); 
		i != objectInfo.importedSymbolOffsets.end(); ++i) 
	{
		std::vector<int>& importOffsets = i->second;

		if (importOffsets.size() > 0) {
			int globalOffset = getGlobalSymbol(objectInfo.fileName, i->first);

			for (int j = 0; j < importOffsets.size(); j++) {
				// Subtract off four because these labels will be used in instructions which are immediates.
				// Shifting back by 4 moves it to target the offset from the instruction as intended.
				int newOffset = globalOffset - (fileOffset + importOffsets[j]);
				uint32_t* binAsIntPtr = (uint32_t*)objectInfo.binary;
				binAsIntPtr[importOffsets[j] / 4] = newOffset;
			}
		}
		else {
			//logger.warning("{s}: Global symbol declared, but not used '{s}'",
				//objectInfo.fileName.c_str(), i->first.c_str());
		}
	}
}

static void loadString(std::ifstream& inputStream, std::string& output) {
	output.clear();

	char byteInput;
	while (!inputStream.eof()) {
		inputStream.read(&byteInput, 1);

		if (byteInput == 0) {
			break;
		}
		else {
			output += byteInput;
		}
	}
}

void Linker::parseOBJ(const std::string& fileName, OBJInfo& info) {
	info.fileName = fileName;

	// Load the header info.
	// Check for the entrypoint.
	std::ifstream inputStream(fileName, std::ios::binary);

	if (inputStream.is_open()) {
		char oneByteInput;
		int fourByteInput;

		inputStream.read(&oneByteInput, 1);

		if (oneByteInput != 0) {
			// We have found an entrypoint
			info.hasEntryPoint = true;
			inputStream.read((char*)&info.entryPointStart, sizeof(uint32_t));
			addEntryPoint(info.fileName, offsets[offsets.size() - 1] + info.entryPointStart);
		}

		// Load exported symbols.
		inputStream.read((char*)&fourByteInput, sizeof(uint32_t));

		for (int i = 0; i < fourByteInput; i++) {
			std::string nextSymbolName;
			loadString(inputStream, nextSymbolName);
			int offset;
			inputStream.read((char*)&offset, sizeof(uint32_t));

			if (info.exportedSymbolOffsets.find(nextSymbolName) == info.exportedSymbolOffsets.end()) {
				info.exportedSymbolOffsets.emplace(nextSymbolName, offset);
				addGlobalSymbol(fileName, nextSymbolName, offset);
			}
			else {
				logger.error("Duplicate declared symbol {s}", nextSymbolName.c_str());
				addError();
			}
		}

		// Load imported symbols
		inputStream.read((char*)&fourByteInput, sizeof(uint32_t));

		for (int i = 0; i < fourByteInput; i++) {
			std::string nextSymbolName;
			loadString(inputStream, nextSymbolName);

			if (info.importedSymbolOffsets.find(nextSymbolName) == info.importedSymbolOffsets.end()) {
				info.importedSymbolOffsets.emplace(nextSymbolName, std::vector<int>());
			}
			else {
				logger.error("Duplicate imported symbol");
				addError();
			}

			int nUses;
			inputStream.read((char*)&nUses, sizeof(uint32_t));

			for (int j = 0; j < nUses; j++) {
				int offset;
				inputStream.read((char*)&offset, sizeof(uint32_t));
				info.importedSymbolOffsets[nextSymbolName].push_back(offset);
			}
		}

		inputStream.read((char*)&info.binarySize, sizeof(uint32_t));

		if (info.binarySize < 0) {
			logger.error("Invalid binary");
			addError();
		}

		info.binary = new char[info.binarySize];
		inputStream.read(info.binary, info.binarySize);
	}
}