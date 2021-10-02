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
	exeGlobalOffsetTable.clear();
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

	// Make sure we have an entrypoint.
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

	// Write data to executable file.
	// Write a jump into the entrypoint (for now). -> later the VM will take care of this.
	if (!hasErrors) {
		std::ofstream outputFile(outputFileName, std::ios::binary | std::ios::out);

		// Entrypoint should be specified as the first byte in the file.
		outputFile.write((char*)&entryPoint, sizeof(uint32_t));

		// Write global offset table for the VM to parse and offset.
		// Start with the size of the GOT.
		int gotSize = exeGlobalOffsetTable.size();
		outputFile.write((char*)&gotSize, sizeof(uint32_t));
		for (int i = 0; i < exeGlobalOffsetTable.size(); i++) {
			int offset = exeGlobalOffsetTable[i];
			outputFile.write((char*)&offset, sizeof(uint32_t));
		}

		// Write the total size of the binary.
		int binarySize = offsets[offsets.size() - 1] + objFileInfo[objFileInfo.size() - 1]->binarySize;
		outputFile.write((char*)&binarySize, sizeof(uint32_t));

		// Write each binary in order.
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
	uint32_t* binAsIntPtr = (uint32_t*)objectInfo.binary;

	// Resolve extern text segment symbols by finding their global offsets.
	for (std::map<std::string, std::vector<int>>::iterator i = 
		objectInfo.importedSymbolOffsetsTextSeg.begin(); 
		i != objectInfo.importedSymbolOffsetsTextSeg.end(); ++i) 
	{
		std::vector<int>& importOffsets = i->second;
		int globalOffset = getGlobalSymbol(objectInfo.fileName, i->first);

		for (int j = 0; j < importOffsets.size(); j++) {
			// Subtract off four because these labels will be used in instructions which are immediates.
			// Shifting back by 4 moves it to target the offset from the instruction as intended.
			int newOffset = globalOffset - (fileOffset + importOffsets[j]);
			binAsIntPtr[importOffsets[j] / sizeof(uint32_t)] = newOffset;
		}
	}

	// Resolve extern data segment symbols by assigning them their global offset.
	for (std::map<std::string, std::vector<int>>::iterator i =
		objectInfo.importedSymbolOffsetsDataSeg.begin();
		i != objectInfo.importedSymbolOffsetsDataSeg.end(); ++i) 
	{
		std::vector<int>& importOffsetsData = i->second;
		int globalOffset = getGlobalSymbol(objectInfo.fileName, i->first);

		for (int j = 0; j < importOffsetsData.size(); j++) {
			binAsIntPtr[importOffsetsData[j] / 4] = globalOffset;
			exeGlobalOffsetTable.push_back(importOffsetsData[j]);
		}
	}

	// Resolve global offset table values by adding the file offset to each byte.
	for (int i = 0; i < objectInfo.globalOffsetTable.size(); i++) {
		int newOffset = fileOffset + binAsIntPtr[objectInfo.globalOffsetTable[i] / sizeof(uint32_t)];
		binAsIntPtr[objectInfo.globalOffsetTable[i] / sizeof(uint32_t)] = newOffset;

		exeGlobalOffsetTable.push_back(objectInfo.globalOffsetTable[i] + fileOffset);
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

		// Load imported symbols in text segment.
		inputStream.read((char*)&fourByteInput, sizeof(uint32_t));

		for (int i = 0; i < fourByteInput; i++) {
			std::string nextSymbolName;
			loadString(inputStream, nextSymbolName);

			if (info.importedSymbolOffsetsTextSeg.find(nextSymbolName) == info.importedSymbolOffsetsTextSeg.end()) {
				info.importedSymbolOffsetsTextSeg.emplace(nextSymbolName, std::vector<int>());
			}

			int nUses;
			inputStream.read((char*)&nUses, sizeof(uint32_t));

			for (int j = 0; j < nUses; j++) {
				int offset;
				inputStream.read((char*)&offset, sizeof(uint32_t));
				info.importedSymbolOffsetsTextSeg[nextSymbolName].push_back(offset);
			}
		}

		// Load imported symbols in the data segment.
		inputStream.read((char*)&fourByteInput, sizeof(uint32_t));

		for (int i = 0; i < fourByteInput; i++) {
			std::string nextSymbolName;
			loadString(inputStream, nextSymbolName);

			if (info.importedSymbolOffsetsDataSeg.find(nextSymbolName) == info.importedSymbolOffsetsDataSeg.end()) {
				info.importedSymbolOffsetsDataSeg.emplace(nextSymbolName, std::vector<int>());
			}

			int nUses;
			inputStream.read((char*)&nUses, sizeof(uint32_t));

			for (int j = 0; j < nUses; j++) {
				int offset;
				inputStream.read((char*)&offset, sizeof(uint32_t));
				info.importedSymbolOffsetsDataSeg[nextSymbolName].push_back(offset);
			}
		}

		// Read global offset table
		int globalOffsetTableSize = 0;
		inputStream.read((char*)&globalOffsetTableSize, sizeof(uint32_t));
		for (int i = 0; i < globalOffsetTableSize; i++) {
			int byteOffset = 0;
			inputStream.read((char*)&byteOffset, sizeof(uint32_t));
			info.globalOffsetTable.push_back(byteOffset);
		}

		// TODO: size error checking.
		// Read binary.
		inputStream.read((char*)&info.binarySize, sizeof(uint32_t));

		if (info.binarySize < 0) {
			logger.error("Invalid binary");
			addError();
		}

		info.binary = new char[info.binarySize];
		inputStream.read(info.binary, info.binarySize);
	}
}