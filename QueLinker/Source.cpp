#include "../DebugLogger.h"
#include "Linker.h"
#include <vector>

/// <summary>
/// The linker's job is to load n obj files into memory, 
/// combine them and adjust the offsets of the imported symbols.
/// Needs to check to make sure that all imported symbols in each file
/// can be resolved.
/// A single file can be linked, but that would simply remove the header
/// and add the jump into the entrypoint.
/// </summary>
/// <param name="argc"></param>
/// <param name="argv"></param>
/// <returns></returns>
int main(int argc, const char** argv) {
	DebugLogger logger("", Level::LEVEL_TRACE);
	logger.setColorDisabled();
	logger.setPrefix("");

	if (argc >= 3) {
		Linker linker;
		std::vector<std::string> fileList;
		std::string outputFileName = std::string(argv[1]);

		for (int i = 2; i < argc; i++) {
			fileList.push_back(std::string(argv[i]));
		}

		if (!linker.linkFiles(outputFileName, fileList)) {
			return -1;
		}
	}
	else {
		logger.error("At least one OBJ file must be specified: \\[outputFileName\\] \\[inputFileName1\\]...");
		return -1;
	}

	return 0;
}