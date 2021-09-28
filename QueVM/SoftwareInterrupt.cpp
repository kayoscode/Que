#include "VirtualMachine.h"

/// When the system receives a software interrupt, it essentially treats it as a function call,
/// but the parameters have to be passed in a very specific way. 
/// When a new system call is added, be sure to document how the parameters should be passed.
/// For example, parameters may be given in any of the registers, but no parameters should be given in
/// memory. However, you can pass the parameters with addresses to memory and that's completely valid.

void SWI_0x00_TERMINATE(VirtualMachine& cpu) noexcept {
	cpu.triggerStop();
}

/// <summary>
/// Printf expects the address of the string to be stored in r0.
/// Following should be a pointer to an array containing each variable to be taken from 
/// memory. The array should contain items in the same order as they appear in the format.
/// </summary>
/// <param name="cpu"></param>
/// <returns></returns>
void SWI_0x01_PRINTF(VirtualMachine& cpu) noexcept {
	// Let's make a very basic printf function which keeps popping values off of stack until 
	// the string has run out of things it's asking for. 
	// Of course this could smash the stack, but only if the programmer 
	// does something idiotic. Who cares?
}

/// <summary>
/// For print int, it will print a single integer to the screen, then add a new line.
/// That integer should be in r0
/// </summary>
/// <param name="cpu"></param>
/// <returns></returns>
void SWI_0x02_PRINT_INT(VirtualMachine& cpu) noexcept {
	std::cout << cpu.mRegisters[VirtualMachine::R0];
}

/// <summary>
/// Character value should be place in r0.
/// Prints it to the screen including a new line.
/// </summary>
/// <param name="cpu"></param>
/// <returns></returns>
void SWI_0x03_PRINT_CHAR(VirtualMachine& cpu) noexcept {
	std::cout << (char)cpu.mRegisters[VirtualMachine::R0];
}

/// <summary>
/// Prints a float to the screen. The float to print should be in F0.
/// </summary>
/// <param name="cpu"></param>
/// <returns></returns>
void SWI_0x04_PRINT_FLOAT(VirtualMachine& cpu) noexcept {
	std::cout << cpu.mFloatRegisters[VirtualMachine::F0];
}

/// <summary>
/// R0 should contain the address of a string to print. It better be null terminated or you could have 
/// some issues. Prints the string to the scree NOT ending the line.
/// </summary>
/// <param name="cpu"></param>
/// <returns></returns>
void SWI_0x04_PRINT_STRING(VirtualMachine& cpu) noexcept {
	std::cout << (cpu.mMemory + cpu.mRegisters[VirtualMachine::R0]);
}

void (*softwareInterruptTable[32])(VirtualMachine& machine) = {
	SWI_0x00_TERMINATE, 
	// Printing software interrupts
	SWI_0x01_PRINTF, SWI_0x02_PRINT_INT, SWI_0x03_PRINT_CHAR,
	SWI_0x04_PRINT_FLOAT, SWI_0x04_PRINT_STRING
};

void handleSWI(VirtualMachine& cpu, int code) noexcept {
	softwareInterruptTable[code](cpu);
}

