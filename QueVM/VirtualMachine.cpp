#include <string>
#include <vector>

#include "VirtualMachine.h"
#include <fstream>

// Define INSTRUCTION_ECHO to view what instructions the VM is executing in the console.
#if defined(_DEBUG)
#define INSTRUCTION_ECHO
#endif

#define INSTRUCTION_SIZE sizeof(uint32_t)
#define IMMEDIATE_SIZE sizeof(uint32_t)

#define GET_OPCODE(instruction) (instruction & 0b1111111)
#define GET_IMMEDIATE_FLAG(instruction) ((instruction & 0b10000000) >> 7)
#define GET_ARG1(instruction) ((instruction & 0b1111100000000) >> 8)
#define GET_ARG2(instruction) ((instruction & 0b111110000000000000) >> 13)
#define GET_ARG3(instruction) ((instruction & 0b11111000000000000000000) >> 18)

// Memory and bit helper macros
#define GETWORD(cpu, location) (((uint32_t*)cpu.mMemory)[(location) / sizeof(uint32_t)])
#define GETHALF(cpu, location) (((uint16_t*)cpu.mMemory)[(location) / sizeof(uint16_t)])
#define GETBYTE(cpu, location) (((uint8_t*)cpu.mMemory)[(location) / sizeof(uint8_t)])

#define SETWORD(cpu, location, value) (((uint32_t*)cpu.mMemory)[(location) / sizeof(uint32_t)] = value)
#define SETHALF(cpu, location, value) (((uint16_t*)cpu.mMemory)[(location) / sizeof(uint16_t)] = value)
#define SETBYTE(cpu, location, value) (((uint8_t*)cpu.mMemory)[(location) / sizeof(uint8_t)] = value)

#define GETFLOAT(cpu, location) (((float*)cpu.mMemory)[(location) / sizeof(float)])

#define FLTTOINT(data) (*(uint32_t*)&data)
#define INTTOFLT(data) (*(float*)&data)

#ifdef INSTRUCTION_ECHO
#define LOGINST(...) do {  \
        cpu.logger.trace(__VA_ARGS__); \
    } while(0)
#else
#define LOGINST
#endif

const char* intRegisterNames[32]{
	"R0", "R1", "R2", "R3",
	"R4", "R5", "R6", "R7",
	"R8", "R9", "R10", "R11",
	"R12", "R13", "R14", "R15",
	"PC", "BP", "SP", "FR", "RA",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID"
};

const char* floatRegisterNames[32]{
	"F0", "F1", "F2", "F3",
	"F4", "F5", "F6", "F7",
	"F8", "F9", "F10", "F11",
	"F12", "F13", "F14", "F15",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID", "INVALID",
	"INVALID", "INVALID", "INVALID",
	"INVALID"
};

void VirtualMachine::printMemoryRange(int start, int end) {
	// Allign to word boundary
	start &= 0xFFFFFFFC;
	end &= 0xFFFFFFFC;

	for (int i = start; i < end; i += 4) {
		uint32_t value = GETWORD((*this), i);
		logger.trace("[7'0x{+X d}:] {12d} {12+d} 0x{8+X d} {c}{c}{c}{c}", 
			i, value, value, value, value & 0xFF, (value & 0xFF00) >> 8,
			(value & 0xFF0000) >> 16, (value & 0xFF000000) >> 24);
	}
}

void VirtualMachine::printRegister(int i, bool flt) {

	if (!flt) {
		int value = mRegisters[i];
		logger.trace("[5'{s}:] {12d} {12+d} 0x{8+X d} {c}",
			intRegisterNames[i], value, value, value, value & 0xFF);
	}
	else {
		logger.trace("[5'{s}:] {f}", floatRegisterNames[i], mFloatRegisters[i]);
	}

}

void VirtualMachine::printFlags() {
	const char* bools[]{ "false", "true" };

	logger.trace("\\[Neg: {s}\\]; \\[Zero: {s}\\]; \\[Carry: {s}\\]; \\[Overflow: {s}\\]",
		bools[isFlagSet(FL_NEGATIVE)],
		bools[isFlagSet(FL_ZERO)],
		bools[isFlagSet(FL_CARRY)],
		bools[isFlagSet(FL_OVERFLOW)]);

}

void INST0x00_NOP(VirtualMachine& cpu) noexcept {
	LOGINST("NOP");
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
}

void INST0x01_LSL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	int32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("LSL {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("LSL {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) << immReg;
	cpu.setZeroAndNeg(cpu.mRegisters[dest]);
}

void INST0x02_LSR(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	int32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("LSR {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("LSR {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) >> immReg;
	cpu.setZeroAndNeg(cpu.mRegisters[dest]);
}

void INST0x03_ADD(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	int32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("ADD {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("ADD {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.setOverFlow(cpu.mRegisters[src], immReg);
	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) + immReg;
	cpu.setZeroAndNeg(cpu.mRegisters[dest]);
}

void INST0x04_SUB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	int32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("SUB {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("SUB {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) - immReg;
	cpu.setZeroAndNeg(cpu.mRegisters[dest]);
}

void INST0x05_MUL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	int32_t immReg = 0;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;
        LOGINST("MUL {s}, {s}, {d}", 
			intRegisterNames[dest], intRegisterNames[src], immReg);
    }
    else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("MUL {s}, {s}, {s}", 
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
    }

	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) * immReg;
	cpu.setZeroAndNeg(cpu.mRegisters[dest]);
}

void INST0x06_DIV(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	int32_t immReg = 0;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;
        LOGINST("DIV {s}, {s}, {d}", 
			intRegisterNames[dest], intRegisterNames[src], immReg);
    }
    else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("DIV {s}, {s}, {s}", 
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
    }

	uint32_t value;

	if (immReg != 0) {
		value = ((int32_t)cpu.mRegisters[src]) / immReg;
	}
	else {
		value = 0;
		cpu.logger.critical("DIVIDE BY ZERO EXECPTION"); // We will handle this differently later !
	}

	cpu.mRegisters[dest] = value;
	cpu.setZeroAndNeg(cpu.mRegisters[dest]);
}

void INST0x35_MOD(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;
        LOGINST("MOD {s}, {s}, {d}", 
			intRegisterNames[dest], intRegisterNames[src], immReg);
    }
    else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("MOD {s}, {s}, {s}", 
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
    }

	uint32_t value;

	if (immReg != 0) {
		value = ((int32_t)cpu.mRegisters[src]) % immReg;
	}
	else {
		value = 0;
		cpu.logger.critical("DIVIDE BY ZERO EXECPTION"); // We will handle this differently later !
	}

	cpu.mRegisters[dest] = value;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x07_ULSL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("ULSL {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("ULSL {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) << immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x08_ULSR(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("ULSL {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("ULSL {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((int32_t)cpu.mRegisters[src]) >> immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x09_UADD(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("UADD {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("UADD {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.setCarry(cpu.mRegisters[src], immReg);
	cpu.mRegisters[dest] = ((uint32_t)cpu.mRegisters[src]) + immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x0A_USUB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("USUB {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("USUB {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((uint32_t)cpu.mRegisters[src]) - immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x0B_AND(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("AND {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("AND {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((uint32_t)cpu.mRegisters[src]) & immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x0C_OR(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("AND {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("AND {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((uint32_t)cpu.mRegisters[src]) | immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x0D_XOR(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("XOR {s}, {s}, {d}",
			intRegisterNames[dest], intRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
		LOGINST("XOR {s}, {s}, {s}",
			intRegisterNames[dest], intRegisterNames[src], intRegisterNames[reg]);
	}

	cpu.mRegisters[dest] = ((uint32_t)cpu.mRegisters[src]) ^ immReg;
	cpu.setZero(cpu.mRegisters[dest]);
}

void INST0x0E_MOV(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

        LOGINST("MOV {s}, {d}", intRegisterNames[dest], immReg);
    }
    else {
		uint8_t reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

        LOGINST("MOV {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
    }

	cpu.mRegisters[dest] = immReg;
}

void INST0x0F_CMP(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	int32_t immReg = 0;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

        LOGINST("CMP {s}, {d}", intRegisterNames[dest], immReg);
    }
    else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

        LOGINST("CMP {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
    }

	int32_t value = (int32_t)cpu.mRegisters[dest] - immReg;
	cpu.setZeroAndNeg(value);
}

/// <summary>
/// LA loads a word from a given address relative to this instruction.
/// </summary>
/// <param name="cpu"></param>
void INST0x10_LA(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// Address is given as an offset from the current instruction. 
	// This instruction is ALWAYS 64 bit, if no immediate is given, the next set of bits can
	// be anything and it won't matter.
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
        LOGINST("LA {s}, {d}", intRegisterNames[dest], (int32_t)immReg);

		uint32_t value = cpu.mRegisters[VirtualMachine::PC] + immReg;
		cpu.mRegisters[dest] = value;
		cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		return;
    }
    else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

        LOGINST("LA {s}, {s}:({d})", intRegisterNames[dest], intRegisterNames[reg], immReg);
    }

	uint32_t value = cpu.mRegisters[VirtualMachine::PC] + immReg;
	cpu.mRegisters[dest] = value;
}

void INST0x11_MOV_GE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVGE {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVGE {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (cpu.isFlagSet(VirtualMachine::FL_ZERO) || !cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
		cpu.mRegisters[dest] = immReg;
	}
}

void INST0x12_MOV_G(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVG {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVG {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (!cpu.isFlagSet(VirtualMachine::FL_NEGATIVE) && !cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
		cpu.mRegisters[dest] = immReg;
	}
}

void INST0x13_MOV_LE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVLE {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVLE {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (cpu.isFlagSet(VirtualMachine::FL_NEGATIVE) || cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
		cpu.mRegisters[dest] = immReg;
	}
}

void INST0x14_MOV_L(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVL {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVL {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (cpu.isFlagSet(VirtualMachine::FL_NEGATIVE) && !cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
		cpu.mRegisters[dest] = immReg;
	}
}

void INST0x15_MOV_E(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVE {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVE {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
		cpu.mRegisters[dest] = immReg;
	}
}

void INST0x16_MOV_NE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVNE {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVNE {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (!cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
		cpu.mRegisters[dest] = immReg;
	}
}

void INST0x17_MOV_C(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t immReg = 0;

	if (immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("MOVNE {s}, {d}", intRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];

		LOGINST("MOVNE {s}, {s}", intRegisterNames[dest], intRegisterNames[reg]);
	}

	if (!cpu.isFlagSet(VirtualMachine::FL_CARRY)) {
		cpu.mRegisters[dest] = immReg;
	}
}

/// <summary>
/// Moves an int register to a floating point register. Performing a cast
/// ITOF has no immediates and the instruction syntax is very simple:
/// (000000) opcode (00000) float register dest (00000) int register src
/// </summary>
/// <param name="cpu"></param>
void INST0x18_MOV_ITOF(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	uint32_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t src = GET_ARG2(cpu.mCurrentInstruction);

	LOGINST("ITOF {s}, {s}", floatRegisterNames[dest], intRegisterNames[src]);

	cpu.mFloatRegisters[dest] = (float)(int32_t)cpu.mRegisters[src];
}

void INST0x36_MOV_UITOF(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	uint32_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t src = GET_ARG2(cpu.mCurrentInstruction);

	LOGINST("UITOF {s}, {s}", floatRegisterNames[dest], intRegisterNames[src]);

	cpu.mFloatRegisters[dest] = (float)(uint32_t)cpu.mRegisters[src];
}

/// <summary>
/// Moves a floating point register to an int register.
/// </summary>
/// <param name="cpu"></param>
void INST0x19_MOV_FTOI(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	uint32_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint32_t src = GET_ARG2(cpu.mCurrentInstruction);

	LOGINST("FTOI {s}, {s}", intRegisterNames[dest], floatRegisterNames[src]);

	int32_t value = (int32_t)cpu.mFloatRegisters[src];
	cpu.mRegisters[dest] = value;
}

void INST0x1A_FADD(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	float immReg = 0;

	if (immediateFlag) {
		immReg = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FADD {s}, {s}, {f}",
			floatRegisterNames[dest], floatRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mFloatRegisters[reg];

		LOGINST("FADD {s}, {s}, {s}",
			floatRegisterNames[dest], floatRegisterNames[src], floatRegisterNames[reg]);
	}

	cpu.mFloatRegisters[dest] = (cpu.mFloatRegisters[src]) + immReg;
	cpu.setZeroAndNeg(cpu.mFloatRegisters[dest]);
}

void INST0x1B_FSUB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	float immReg = 0;

	if (immediateFlag) {
		immReg = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FSUB {s}, {s}, {f}",
			floatRegisterNames[dest], floatRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mFloatRegisters[reg];

		LOGINST("FSUB {s}, {s}, {s}",
			floatRegisterNames[dest], floatRegisterNames[src], floatRegisterNames[reg]);
	}

	cpu.mFloatRegisters[dest] = (cpu.mFloatRegisters[src]) - immReg;
	cpu.setZeroAndNeg(cpu.mFloatRegisters[dest]);
}

void INST0x1C_FMUL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	float immReg = 0;

	if (immediateFlag) {
		immReg = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FMUL {s}, {s}, {f}",
			floatRegisterNames[dest], floatRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mFloatRegisters[reg];

		LOGINST("FMUL {s}, {s}, {s}",
			floatRegisterNames[dest], floatRegisterNames[src], floatRegisterNames[reg]);
	}

	cpu.mFloatRegisters[dest] = (cpu.mFloatRegisters[src]) * immReg;
	cpu.setZeroAndNeg(cpu.mFloatRegisters[dest]);
}

void INST0x1D_FDIV(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG2(cpu.mCurrentInstruction);
	float immReg = 0;

	if (immediateFlag) {
		immReg = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FDIV {s}, {s}, {f}",
			floatRegisterNames[dest], floatRegisterNames[src], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mFloatRegisters[reg];

		LOGINST("FDIV {s}, {s}, {s}",
			floatRegisterNames[dest], floatRegisterNames[src], floatRegisterNames[reg]);
	}

	cpu.mFloatRegisters[dest] = (cpu.mFloatRegisters[src]) / immReg;
	cpu.setZeroAndNeg(cpu.mFloatRegisters[dest]);
}

void INST0x1E_FCMP(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	float immReg = 0;

	if (immediateFlag) {
		immReg = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FCMP {s}, {f}",
			floatRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG3(cpu.mCurrentInstruction);
		immReg = cpu.mFloatRegisters[reg];

		LOGINST("FCMP {s}, {s}",
			floatRegisterNames[dest], floatRegisterNames[reg]);
	}

	float value = cpu.mFloatRegisters[dest] - immReg;
	cpu.setZeroAndNeg(value);
}

void INST0x1F_FMOV(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	// 000000(op) 0(immflag) 00000 (dest) 00000 (src)
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	float immReg = 0;

	if (immediateFlag) {
		immReg = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FMOV {s}, {f}",
			floatRegisterNames[dest], immReg);
	}
	else {
		int reg = GET_ARG2(cpu.mCurrentInstruction);
		immReg = cpu.mFloatRegisters[reg];

		LOGINST("FMOV {s}, {s}",
			floatRegisterNames[dest], floatRegisterNames[reg]);
	}

	cpu.mFloatRegisters[dest] = immReg;
}

/// <summary>
/// Stores a floating point value in memory
/// 000000 opcode 1 offsetimmediateflag 00000 float register src 00000 int register address
/// </summary>
/// <param name="cpu"></param>
void INST0x20_FS(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FS {s}, {s}, {d}", floatRegisterNames[src], intRegisterNames[address], offset);
	}
	else {
		LOGINST("FS {s}, {s}", floatRegisterNames[src], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	float value = cpu.mFloatRegisters[src];
	uint32_t ieee = FLTTOINT(value);
	SETWORD(cpu, location, ieee);
}

void INST0x21_FL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("FS {s}, {s}\\[{d}\\]", floatRegisterNames[src], intRegisterNames[address], offset);
	}
	else {
		LOGINST("FS {s}, {s}", floatRegisterNames[src], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	float value = GETFLOAT(cpu, location);
	cpu.mFloatRegisters[src] = value;
}

void INST0x22_SB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("SB {s}, {s}\\[{d}\\]", intRegisterNames[src], intRegisterNames[address], offset);
	}
	else {
		LOGINST("SB {s}, {s}", intRegisterNames[src], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	uint8_t value = cpu.mRegisters[src] & 0xFF;
	SETBYTE(cpu, location, value);
}

void INST0x23_SH(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("SH {s}, {s}\\[{d}\\]", intRegisterNames[src], intRegisterNames[address], offset);
	}
	else {
		LOGINST("SH {s}, {s}", intRegisterNames[src], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	uint16_t value = cpu.mRegisters[src] & 0xFFFF;
	SETHALF(cpu, location, value);
}

void INST0x24_SW(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t src = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("SW {s}, {s}\\[{d}\\]", intRegisterNames[src], intRegisterNames[address], offset);
	}
	else {
		LOGINST("SW {s}, {s}", intRegisterNames[src], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	uint32_t value = cpu.mRegisters[src];
	SETWORD(cpu, location, value);
}

void INST0x25_LB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("LB {s}, {s}\\[{d}\\]", intRegisterNames[dest], intRegisterNames[address], offset);
	}
	else {
		LOGINST("LB {s}, {s}", intRegisterNames[dest], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	cpu.mRegisters[dest] = GETBYTE(cpu, location);
}

void INST0x26_LH(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("LH {s}, {s}\\[{d}\\]", intRegisterNames[dest], intRegisterNames[address], offset);
	}
	else {
		LOGINST("LH {s}, {s}", intRegisterNames[dest], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	cpu.mRegisters[dest] = GETHALF(cpu, location);
}

void INST0x27_LW(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint8_t dest = GET_ARG1(cpu.mCurrentInstruction);
	uint8_t address = GET_ARG2(cpu.mCurrentInstruction);
	int32_t offset = 0;

	if (immediateFlag) {
		offset = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += IMMEDIATE_SIZE;

		LOGINST("LW {s}, {s}\\[{d}\\]", intRegisterNames[dest], intRegisterNames[address], offset);
	}
	else {
		LOGINST("LW {s}, {s}", intRegisterNames[dest], intRegisterNames[address]);
	}

	uint32_t location = cpu.mRegisters[address] + offset;
	cpu.mRegisters[dest] = GETWORD(cpu, location);
}

/// <summary>
/// Jump instructions have two different modes:
/// absolute mode and relative mode.
/// If an immediate is given, the mode is relative (adds a value to the instruction ptr).
/// If a register is given, the mode is absolute (sets the instruction ptr to a value).
/// 000000 opcode 0 immediateflag 00000 register
/// </summary>
/// <param name="cpu"></param>
void INST0x28_JMP(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JMP {d}", immReg);
		cpu.mRegisters[VirtualMachine::PC] += immReg;
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JMP {s}", intRegisterNames[reg]);
		cpu.mRegisters[VirtualMachine::PC] = immReg;
    }
}

void INST0x29_CALL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		// Set the return address to the next instruction
		cpu.mRegisters[VirtualMachine::RA] = cpu.mRegisters[VirtualMachine::PC] + INSTRUCTION_SIZE;
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("CALL {d}", immReg);
		cpu.mRegisters[VirtualMachine::PC] += immReg;
    }
    else {
		// Set the return address to the next instruction
		cpu.mRegisters[VirtualMachine::RA] = cpu.mRegisters[VirtualMachine::PC];

		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("CALL {s}", intRegisterNames[reg]);
		cpu.mRegisters[VirtualMachine::PC] = immReg;
    }
}

void INST0x2A_JGE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JGE {d}", immReg);

		if (cpu.isFlagSet(VirtualMachine::FL_ZERO) || !cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JGE {s}", intRegisterNames[reg]);

		if (cpu.isFlagSet(VirtualMachine::FL_ZERO) || !cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

void INST0x2B_JG(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JG {d}", immReg);

		if (!cpu.isFlagSet(VirtualMachine::FL_ZERO) && !cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JG {s}", intRegisterNames[reg]);

		if (!cpu.isFlagSet(VirtualMachine::FL_ZERO) && !cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

void INST0x2C_JLE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JLE {d}", immReg);

		if (cpu.isFlagSet(VirtualMachine::FL_ZERO) || cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JLE {s}", intRegisterNames[reg]);

		if (cpu.isFlagSet(VirtualMachine::FL_ZERO) || cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

void INST0x2D_JL(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JL {d}", immReg);

		if (!cpu.isFlagSet(VirtualMachine::FL_ZERO) && cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JL {s}", intRegisterNames[reg]);

		if (!cpu.isFlagSet(VirtualMachine::FL_ZERO) && cpu.isFlagSet(VirtualMachine::FL_NEGATIVE)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

void INST0x2E_JE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JE {d}", immReg);

		if (cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JE {s}", intRegisterNames[reg]);

		if (cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

void INST0x2F_JNE(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JNE {d}", immReg);

		if (!cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JNE {s}", intRegisterNames[reg]);

		if (!cpu.isFlagSet(VirtualMachine::FL_ZERO)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

void INST0x30_JC(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	int32_t immReg;

    if(immediateFlag) {
		immReg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);

        LOGINST("JC {d}", immReg);

		if (!cpu.isFlagSet(VirtualMachine::FL_CARRY)) {
			cpu.mRegisters[VirtualMachine::PC] += immReg;
		}
		else {
			cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		}
    }
    else {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immReg = cpu.mRegisters[reg];
        LOGINST("JC {s}", intRegisterNames[reg]);

		if (!cpu.isFlagSet(VirtualMachine::FL_CARRY)) {
			cpu.mRegisters[VirtualMachine::PC] = immReg;
		}
    }
}

/// <summary>
/// Decrements the stack pointer the appropriate amount, then writes the item to it
/// 000000 opcode 00000 register
/// </summary>
/// <param name="cpu"></param>
void INST0x31_PUSHB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	uint8_t immreg = 0;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);

	if (!immediateFlag) {
		uint32_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immreg = cpu.mRegisters[reg] & 0xFF;
		LOGINST("PUSHB {s}", intRegisterNames[reg]);
	}
	else {
		immreg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]) & 0xFF;
		cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		LOGINST("PUSHB {d}", immreg);
	}

	cpu.mRegisters[VirtualMachine::SP] -= 1;
	SETBYTE(cpu, cpu.mRegisters[VirtualMachine::SP], immreg);
}

void INST0x32_POPB(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("POPB {s}", intRegisterNames[reg]);

	cpu.mRegisters[reg] = GETBYTE(cpu, cpu.mRegisters[VirtualMachine::SP]);
	cpu.mRegisters[VirtualMachine::SP] += 1;
}

void INST0x37_PUSHH(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	uint16_t immreg = 0;
	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);

	if (!immediateFlag) {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immreg = cpu.mRegisters[reg] & 0xFFFF;
		LOGINST("PUSHH {s}", intRegisterNames[reg]);
	}
	else {
		immreg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]) & 0xFFFF;
		cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		LOGINST("PUSHH {d}", immreg);
	}

	cpu.mRegisters[VirtualMachine::SP] -= 2;
	SETHALF(cpu, cpu.mRegisters[VirtualMachine::SP], immreg);
}

void INST0x38_PUSHW(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	bool immediateFlag = GET_IMMEDIATE_FLAG(cpu.mCurrentInstruction);
	uint32_t immreg = 0;

	if (!immediateFlag) {
		uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);
		immreg = cpu.mRegisters[reg];

		LOGINST("PUSHW {s}", intRegisterNames[reg]);
	}
	else {
		immreg = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC]);
		cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
		LOGINST("PUSHW {d}", immreg);
	}

	cpu.mRegisters[VirtualMachine::SP] -= 4;
	SETWORD(cpu, cpu.mRegisters[VirtualMachine::SP], immreg);
}

void INST0x39_POPH(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("POPH {s}", intRegisterNames[reg]);

	cpu.mRegisters[reg] = GETHALF(cpu, cpu.mRegisters[VirtualMachine::SP]);
	cpu.mRegisters[VirtualMachine::SP] += 2;
}

void INST0x3A_POPW(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("POPW {s}", intRegisterNames[reg]);

	cpu.mRegisters[reg] = GETWORD(cpu, cpu.mRegisters[VirtualMachine::SP]);
	cpu.mRegisters[VirtualMachine::SP] += 4;
}

void INST0x3B_PUSHF(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("PUSHF {s}", floatRegisterNames[reg]);

	cpu.mRegisters[VirtualMachine::SP] -= 4;
	float value = cpu.mFloatRegisters[reg];
	uint32_t ieee = FLTTOINT(value);
	SETWORD(cpu, cpu.mRegisters[VirtualMachine::SP], value);
}

void INST0x3C_POPF(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("POPF {s}", floatRegisterNames[reg]);

	cpu.mFloatRegisters[reg] = GETFLOAT(cpu, cpu.mRegisters[VirtualMachine::SP]);
	cpu.mRegisters[VirtualMachine::SP] += 4;
}

void INST0x3D_INC(VirtualMachine& cpu) noexcept {
	// 000000 opcode 00000 reg
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("INC {s}", intRegisterNames[reg]);

	cpu.mRegisters[reg]++;
	cpu.setZeroAndNeg(cpu.mRegisters[reg]);
}

void INST0x3E_DEC(VirtualMachine& cpu) noexcept {
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;

	uint8_t reg = GET_ARG1(cpu.mCurrentInstruction);

	LOGINST("DEC {s}", intRegisterNames[reg]);

	cpu.mRegisters[reg]--;
	cpu.setZeroAndNeg(cpu.mRegisters[reg]);
}

void INST0x34_HWI(VirtualMachine& cpu) noexcept {
	cpu.triggerStop();
	LOGINST("HWI");
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
}

void INST0xXX_ERR(VirtualMachine& cpu) noexcept {
	// an illegal instruction has been called.
	// Normally this would just be a program exception rather than halting the entire VM, but this will do for now.
	cpu.triggerStop();
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE;
	LOGINST("INVALID INSTRUCTION");
}

extern void HANDLE_SWI(VirtualMachine& cpu) noexcept;

void INST0x33_SWI(VirtualMachine& cpu) noexcept {
	LOGINST("SWI");
	uint32_t imm = GETWORD(cpu, cpu.mRegisters[VirtualMachine::PC] + INSTRUCTION_SIZE);
	cpu.mRegisters[VirtualMachine::PC] += INSTRUCTION_SIZE * 2;

	handleSWI(cpu, imm);
}

/// <summary>
/// Maps an opcode to a resolution function.
/// </summary>
void (*instructionFunctions[64])(VirtualMachine& machine) = {
	// Basic ALU instructions.
	INST0x00_NOP, INST0x01_LSL, INST0x02_LSR,
	INST0x03_ADD, INST0x04_SUB, INST0x05_MUL,
	INST0x06_DIV, INST0x07_ULSL, INST0x08_ULSR,
	INST0x09_UADD, INST0x0A_USUB, INST0x0B_AND,
	INST0x0C_OR, INST0x0D_XOR,

	// Basic logic and assignment instructions.
	INST0x0E_MOV, INST0x0F_CMP, INST0x10_LA,
	INST0x11_MOV_GE, INST0x12_MOV_G, INST0x13_MOV_LE,
	INST0x14_MOV_L, INST0x15_MOV_E, INST0x16_MOV_NE,
	INST0x17_MOV_C,

	// Conversions.
	INST0x18_MOV_ITOF, INST0x19_MOV_FTOI,

	// Float point.
	INST0x1A_FADD, INST0x1B_FSUB,
	INST0x1C_FMUL, INST0x1D_FDIV,
	INST0x1E_FCMP, INST0x1F_FMOV,
	INST0x20_FS, INST0x21_FL,

	// Memory.
	INST0x22_SB, INST0x23_SH, INST0x24_SW,
	INST0x25_LB, INST0x26_LH, INST0x27_LW,

	// Jumps.
	INST0x28_JMP, INST0x29_CALL, INST0x2A_JGE,
	INST0x2B_JG, INST0x2C_JLE, INST0x2D_JL,
	INST0x2E_JE, INST0x2F_JNE, INST0x30_JC,

	// Stack.
	INST0x31_PUSHB, INST0x32_POPB,

	// Interrupts
	INST0x33_SWI, INST0x34_HWI, INST0x35_MOD,
	INST0x36_MOV_UITOF, 

	INST0x37_PUSHH, INST0x38_PUSHW,
	INST0x39_POPH, INST0x3A_POPW,
	INST0x3B_PUSHF, INST0x3C_POPF,

	// INC/DEC
	INST0x3D_INC, INST0x3E_DEC,

	// Invalid instructions.
	INST0xXX_ERR, 
};

VirtualMachine::VirtualMachine(unsigned int memorySize) :
	mMemorySize(memorySize),
	logger("", Level::LEVEL_TRACE)
{
	logger.setColorDisabled();
	for (int i = 0; i < INT_REGISTER_COUNT; i++) {
		logger.addVariable(std::string(intRegisterNames[i]), &mRegisters[i], DebugVarType::INTEGER32);
	}

	for (int i = 0; i < FP_REGISTER_COUNT; i++) {
		logger.addVariable(std::string(floatRegisterNames[i]), &mFloatRegisters[i], DebugVarType::FLOAT32);
	}

	logger.setPrefix("[012'[.2tm] 0x[X PC]:] ");

	// Allocate memory for internal memory state.
	// Allocate as uint32_t ptr to ensure its on a word boundary and sized to a word.
	mMemory = (uint8_t*)new uint32_t[memorySize / 4];

	initState();
}

void VirtualMachine::initState() {
	// Initialize registers to zero
	for (int i = 0; i < Int32Register::INT_REGISTER_COUNT; i++) {
		mRegisters[i] = 0;
	}

	for (int i = 0; i < Float32Register::FP_REGISTER_COUNT; i++) {
		mFloatRegisters[i] = 0.0f;
	}
}

uint32_t VirtualMachine::loadProgram(const std::string& fileName) {
	// Parse the executable's header.
	std::ifstream fileInputStream(fileName, std::ios::binary | std::ios::in);
	
	if (fileInputStream.is_open()) {
		fileInputStream.read((char*)&mProgramEntryPoint, sizeof(uint32_t));

		int gotSize = 0;
		fileInputStream.read((char*)&gotSize, sizeof(uint32_t));
		int* gotTable = new int[gotSize];

		for (int i = 0; i < gotSize; i++) {
			fileInputStream.read((char*)&gotTable[i], sizeof(uint32_t));
		}

		uint32_t binarySize = 0;
		fileInputStream.read((char*)&binarySize, sizeof(uint32_t));

		// Determine where the file should be written into memory.
		// We will write it to the back of the mem.
		int programStart = 0;

		fileInputStream.read((char*)mMemory + programStart, binarySize);

		// Offset each element in the binary's global offset table by the program start.
		// Program start better be aligned to a word bounadary!!
		uint32_t* memAsInt = (uint32_t*)mMemory;
		for (int i = 0; i < gotSize; i++) {
			memAsInt[(gotTable[i] + programStart) / (int)sizeof(uint32_t)] += programStart;
		}
		mProgramEntryPoint += programStart;

		delete[] gotTable;
		return binarySize;
	}
	else {
		return -1;
	}
}

void VirtualMachine::executeProgram(int stackPtr) {
	setRunning();
	mRegisters[PC] = mProgramEntryPoint;
	mRegisters[SP] = stackPtr;

	uint64_t count = 0;
	// Execution completes when an instruction signals stop to the VM.
	while (!mSignalStop) {
		executeNextInstruction();
		count += 1;
	}

	//logger.trace("{long} instructions executed in [tl] milliseconds", count);
}

void VirtualMachine::executeNextInstruction() {
	// The first 7 bits in the number indicates the opcode.
	mCurrentInstruction = GETWORD((*this), mRegisters[PC]);

	uint32_t opcode = GET_OPCODE(mCurrentInstruction);

	instructionFunctions[opcode](*this);
}

VirtualMachine::~VirtualMachine() {
	// Free memory used by VM.
	if (mMemory == nullptr) {
		delete[] mMemory;
	}
}