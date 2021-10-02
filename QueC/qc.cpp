#include "../DebugLogger.h"
#include "Compiler.h"
#include <string>
#include <fstream>

/// <summary>
/// Que compiler. The job is to take que source code and convert
/// it into either asm or an object file to be linked. Just like the assembler.
/// NOTE TO SELF: Potentially, I might have to change the linker to declare 
/// What the offset to an external label should start from, rather than from that
/// specific word it's replacing. Food for thought.
/// </summary>
/// <returns></returns>
int main(int argc, const char** argv) {
	// For now, load a static file.

	DebugLogger out("", Level::LEVEL_TRACE);
	out.setPrefix("");
	out.setColorDisabled();

	if (argc == 3) {
		std::string fileName = std::string(argv[1]);
		std::string outputFileName = std::string(argv[2]);
		Compiler compiler;
		char* outputBuffer = nullptr;
		int binarySize = 0;

		std::ofstream outputFile(outputFileName, std::ios::out | std::ios::binary);
		if (!compiler.compileFile(fileName, outputFile)) {
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