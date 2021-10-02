#include "Assembler.h"
#include <fstream>
#include <stdlib.h>

/// <summary>
/// The assembler's job is convert an asm file into an object
/// file. Therefore, the only command line argument is the filePath followed by 
/// the resulting file path.
/// </summary>
/// <returns></returns>
int main(int argc, char** argv) {
	DebugLogger out("", Level::LEVEL_TRACE);
	out.setPrefix("");
	out.setColorDisabled();

	if (argc == 3) {
		std::string fileName = std::string(argv[1]);
		std::string outputFileName = std::string(argv[2]);
		Assembler assembler;
		char* outputBuffer = nullptr;
		int binarySize = 0;

		std::ofstream outputFile(outputFileName, std::ios::out | std::ios::binary);
		if (!assembler.assembleFile(fileName, outputFile)) {
			outputFile.close();
			return -1;
		}
		outputFile.close();
	}
	else {
		out.critical("Missing args: \\[inputFileName\\] \\[outputFileName\\]");
		return -1;
	}

	return 0;
}