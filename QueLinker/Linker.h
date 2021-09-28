#pragma once

#include "../DebugLogger.h"
#include <vector>
#include <map>
#include <string>

class Linker {
public:
	Linker();
	~Linker();

	/// <summary>
	/// Links the obj files given by the user.
	/// </summary>
	/// <param name="files"></param>
	bool linkFiles(const std::string& outputFile, const std::vector<std::string>& files);

	/// <summary>
	/// Contains loaded header information from an obj file.
	/// </summary>
	struct OBJInfo {
		OBJInfo() 
			:hasEntryPoint(false),
			entryPointStart(0),
			exportedSymbolOffsets(),
			importedSymbolOffsets(),
			binarySize(0),
			binary(nullptr)
		{
		}

		~OBJInfo() {
			delete[] binary;
		}

		bool hasEntryPoint;
		int entryPointStart;
		std::string fileName;
		std::map<std::string, int> exportedSymbolOffsets;
		std::map<std::string, std::vector<int>> importedSymbolOffsets;
		int binarySize;
		char* binary;
	};

	/// <summary>
	/// Parses header information.
	/// </summary>
	/// <param name="fileName"></param>
	/// <param name="objectInfo"></param>
	void parseOBJ(const std::string& fileName, OBJInfo& objectInfo);

	/// <summary>
	/// Resolves all symbols in that obj file's binary.
	/// </summary>
	/// <param name="objectInfo"></param>
	void resolveSymbols(OBJInfo& objectInfo, int fileOffset);

	void addError() {
		hasErrors = true;
	}

	/// <summary>
	/// Adds a global symbol to the linker state.
	/// </summary>
	/// <param name="originalFile"></param>
	/// <param name="symbolName"></param>
	/// <param name="localOffset"></param>
	void addGlobalSymbol(const std::string& originalFile, const std::string& symbolName, int localOffset) {
		std::map<std::string, int>::iterator i = globalSymbols.find(symbolName);

		if (i == globalSymbols.end()) {
			globalSymbols.emplace(symbolName, offsets[offsets.size() - 1] + localOffset);
			globalSymbolsOriginalFile[symbolName] = originalFile;
		}
		else {
			addError();
			logger.error("{s}: Global symbol '{s}' defined multiple times. Previous defined in {s}",
				originalFile.c_str(), symbolName.c_str(), globalSymbolsOriginalFile[symbolName].c_str());
		}
	}

	/// <summary>
	/// Returns the global offset for the defined global symbol.
	/// </summary>
	/// <param name="originalFile"></param>
	/// <param name="symbolName"></param>
	/// <returns></returns>
	int getGlobalSymbol(const std::string& originalFile, const std::string& symbolName) {
		std::map<std::string, int>::iterator i = globalSymbols.find(symbolName);

		if (i != globalSymbols.end()) {
			return i->second;
		}
		else {
			addError();
			logger.error("{s}: Unresolved global symbol '{s}'",
				originalFile.c_str(), symbolName.c_str());

			return 0;
		}
	}

	void addEntryPoint(const std::string& fileName, int entryPoint) {
		if (this->entryPoint == -1) {
			this->entryPoint = entryPoint;
			this->entrypointFile = fileName;
		}
		else {
			addError();
			logger.error("{s}: More than one entrypoint defined. Previously defined in {s}", fileName.c_str(), entrypointFile.c_str());
		}
	}

protected:
private:
	DebugLogger logger;
	bool hasErrors = false;
	std::vector<int> offsets;
	std::map<std::string, int> globalSymbols;
	std::map<std::string, std::string> globalSymbolsOriginalFile;
	int entryPoint = -1;
	std::string entrypointFile;
};
