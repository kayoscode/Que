#include <iostream>

#include "VirtualMachine.h"
#include <fstream>

/**
 * Undefine INSTRUCTION_ECHO at the top of VirtualMachine.cpp to disable printing
 * each executed instruction!
*/

int main(int argc, const char** argv) {
	int vmMemorySize = 1000000;

	if (argc > 1) {
		if (argc > 3) {
			if (std::string(argv[2]) == "-m") {
				try {
					std::string memSize = argv[3];
					vmMemorySize = std::stoi(memSize);
				}
				catch (std::exception) {
					std::cout << "-m memorySize expected";
					return -1;
				}
			}
		}

		std::string binaryPath = std::string(argv[1]);

		VirtualMachine vm(vmMemorySize);

		std::ifstream input(binaryPath, std::ios::in | std::ios::binary | std::ios::ate);

		if (input.is_open()) {
			vm.loadProgram(binaryPath);

			vm.executeProgram(10000);
			//vm.printRegisterState();
			//vm.printFlags();
			//vm.printMemoryRange(2000, 0x82C);
			input.close();
		}
	}

	return 0;
}