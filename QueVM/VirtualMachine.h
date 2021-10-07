#pragma once

#include <stdint.h>
#include <iostream>
#include "../DebugLogger.h"

typedef uint32_t reg32;
typedef float regf32;

/// <summary>
/// Class responsible for holding the state of the virtual machine 
/// and properly updating it as instructions are executed.
/// </summary>
class VirtualMachine {
public:
	/// <summary>
	/// Defines the 32 bit registers available to the user.
	/// R0-R15: random use registers.
	/// PC: program counter register.
	/// BP: base stack pointer.
	/// SP: stack pointer.
	/// FR: flag register.
	/// RA: return address.
	/// </summary>
	enum Int32Register {
		R0, R1, R2, R3,
		R4, R5, R6, R7,
		R8, R9, R10, R11,
		R12, R13, R14, R15,
		PC, BP, SP, FR, RA,
		INT_REGISTER_COUNT
	};

	/// <summary>
	/// Defines the 32 bit floating point registers available.
	/// F0-F15: random use floating point registers.
	/// </summary>
	enum Float32Register {
		F0, F1, F2, F3,
		F4, F5, F6, F7,
		F8, F9, F10, F11,
		F12, F13, F14, F15,
		FP_REGISTER_COUNT
	};

	/// <summary>
	/// List of flags in the flag register.
	/// </summary>
	enum REGISTER_FLAGS {
		FL_NEGATIVE = 1,
		FL_ZERO = 2,
		FL_CARRY = 4,
		FL_OVERFLOW = 8,
		FL_ENABLE_INTERRUPT = 16
	};

	/// <summary>
	/// Standard constructor
	/// </summary>
	/// <param name="memorySize"></param>
	VirtualMachine(unsigned int memorySize);

	~VirtualMachine();

	reg32 getRegisterState(Int32Register reg) {
		return mRegisters[reg];
	}

	regf32 getFloatRegisterState(Float32Register reg) {
		return mFloatRegisters[reg];
	}

	void setRegisterState(Int32Register reg, reg32 value) {
		mRegisters[reg] = value;
	}

	void setFloatRegisterState(Float32Register reg, regf32 value) {
		mFloatRegisters[reg] = value;
	}

	/// <summary>
	/// Returns whether an instruction has signaled to stop
	/// </summary>
	/// <returns></returns>
	bool getStopTriggered() {
		return mSignalStop;
	}

	/// <summary>
	/// Sets VM in the execution state
	/// </summary>
	void setRunning() {
		mSignalStop = false;
	}

	/// <summary>
	/// Loads binary for a new program into memory.
	/// </summary>
	/// <param name="binary"></param>
	/// <param name="size"></param>
	/// <param name="index"></param>
	/// <return>The size of the executable binary.</return>
	uint32_t loadProgram(const std::string& fileName);

	/// <summary>
	/// Executes a program which has already been loaded
	/// into memory.
	/// Start index must be the same index as the index the program was loaded into
	/// </summary>
	/// <param name="index"></param>
	void executeProgram(int stackPtr);

	/// <summary>
	/// Executes the next instruction in memory and adjusts the Program counter.
	/// </summary>
	void executeNextInstruction();

	/// <summary>
	/// Prints all flags to console.
	/// </summary>
	void printFlags();

	/// <summary>
	/// Prints the state of a single register.
	/// </summary>
	/// <param name="i"></param>
	void printRegister(int i, bool flt);

	/// <summary>
	/// Prints memory within a specified range
	/// </summary>
	/// <param name="start"></param>
	/// <param name="end"></param>
	void printMemoryRange(int start, int end);

	/// <summary>
	/// Prints the state of all registers.
	/// </summary>
	void printRegisterState() {
		for (int i = 0; i < INT_REGISTER_COUNT; i++) {
			printRegister(i, false);
		}

		for (int i = 0; i < FP_REGISTER_COUNT; i++) {
			printRegister(i, true);
		}
	}

private:
	/// <summary>
	/// Initializes the state of the virtual machine
	/// clearing memory, and registers to zero
	/// </summary>
	void initState();

	/// <summary>
	/// Terminate the program
	/// </summary>
	void triggerStop() {
		mSignalStop = true;
	}

	/// <summary>
	/// The current register state
	/// </summary>
	reg32 mRegisters[Int32Register::INT_REGISTER_COUNT];
	regf32 mFloatRegisters[Float32Register::FP_REGISTER_COUNT];

	/// <summary>
	/// Holds the current memory state of the system.
	/// </summary>
	uint8_t* mMemory = nullptr;
	int mMemorySize = 0;
	bool mSignalStop = false;

	uint32_t mCurrentInstruction = 0;
	uint32_t mProgramEntryPoint = 0;

	/// <summary>
	/// Sets flags zero and negative based on result.
	/// </summary>
	/// <param name="result1"></param>
	inline void setZeroAndNeg(uint32_t result) {
		setZeroAndNeg((int32_t)result);
	}

	/// <summary>
	/// Sets flags zero and negative based on result.
	/// </summary>
	/// <param name="result1"></param>
	inline void setZeroAndNeg(int32_t result) {
		if (result == 0) {
			setFlag(FL_ZERO);
		}
		else {
			clearFlag(FL_ZERO);
		}

		if (result < 0) {
			setFlag(FL_NEGATIVE);
		}
		else {
			clearFlag(FL_NEGATIVE);
		}
	}

	/// <summary>
	/// Sets the zero and negative flag 
	/// based on a floating point result.
	/// </summary>
	/// <param name="result"></param>
	inline void setZeroAndNeg(float result) {
		if (result < 1e-10 && result > -1e-10) {
			setFlag(FL_ZERO);
		}
		else {
			clearFlag(FL_ZERO);
		}

		if (result < 0) {
			setFlag(FL_NEGATIVE);
		}
		else {
			clearFlag(FL_NEGATIVE);
		}
	}

	/// <summary>
	/// Sets the zero flag based on result.
	/// </summary>
	/// <param name="result"></param>
	inline void setZero(uint32_t result) {
		if (result == 0) {
			setFlag(FL_ZERO);
		}
		else {
			clearFlag(FL_ZERO);
		}
	}

	/// <summary>
	/// Sets the overflow flag based on two int32 operands
	/// </summary>
	/// <param name="operand1"></param>
	/// <param name="operand2"></param>
	inline void setOverFlow(int32_t operand1, int32_t operand2) {
		int64_t answer = (int64_t)operand1 + (int64_t)operand2;
		int32_t answer2 = (int32_t)answer;

		if (answer == answer2) {
			clearFlag(FL_OVERFLOW);
		}
		else {
			setFlag(FL_OVERFLOW);
		}
	}

	/// <summary>
	/// Sets the carry flag based on two operands
	/// </summary>
	/// <param name="operand1"></param>
	/// <param name="operand2"></param>
	inline void setCarry(uint32_t operand1, uint32_t operand2) {
		uint64_t answer = (uint32_t)operand1 + (uint64_t)operand2;
		uint32_t answer2 = (uint32_t)answer;

		if (answer == answer2) {
			clearFlag(FL_CARRY);
		}
		else {
			setFlag(FL_CARRY);
		}
	}

	/// <summary>
	/// Checks if a specific flag is set.
	/// </summary>
	/// <param name="flag"></param>
	/// <returns></returns>
	inline bool isFlagSet(uint32_t flag) {
		return ((mRegisters[FR] & flag) != 0);
	}

	/// <summary>
	/// Sets a flag.
	/// </summary>
	/// <param name="flag"></param>
	inline void setFlag(uint32_t flag) {
		mRegisters[FR] |= flag;
	}

	/// <summary>
	/// Clears a specific flag.
	/// </summary>
	/// <param name="flag"></param>
	inline void clearFlag(uint32_t flag) {
		mRegisters[FR] &= ~(flag);
	}

	// Logger for recording events during vm runtime.
	DebugLogger logger;

	/// <summary>
	/// Strategies for handling each opcode.
	/// </summary>
	/// <param name="cpu"></param>
	friend void INST0x00_NOP(VirtualMachine& cpu) noexcept;
	friend void INST0x01_LSL(VirtualMachine& cpu) noexcept;
	friend void INST0x02_LSR(VirtualMachine& cpu) noexcept;
	friend void INST0x03_ADD(VirtualMachine& cpu) noexcept;
	friend void INST0x04_SUB(VirtualMachine& cpu) noexcept;
	friend void INST0x05_MUL(VirtualMachine& cpu) noexcept;
	friend void INST0x06_DIV(VirtualMachine& cpu) noexcept;
	friend void INST0x07_ULSL(VirtualMachine& cpu) noexcept;
	friend void INST0x08_ULSR(VirtualMachine& cpu) noexcept;
	friend void INST0x09_UADD(VirtualMachine& cpu) noexcept;
	friend void INST0x0A_USUB(VirtualMachine& cpu) noexcept;
	friend void INST0x0B_AND(VirtualMachine& cpu) noexcept;
	friend void INST0x0C_OR(VirtualMachine& cpu) noexcept;
	friend void INST0x0D_XOR(VirtualMachine& cpu) noexcept;

	friend void INST0x0E_MOV(VirtualMachine& cpu) noexcept;
	friend void INST0x0F_CMP(VirtualMachine& cpu) noexcept;
	friend void INST0x10_LA(VirtualMachine& cpu) noexcept;
	friend void INST0x11_MOV_GE(VirtualMachine& cpu) noexcept;
	friend void INST0x12_MOV_G(VirtualMachine& cpu) noexcept;
	friend void INST0x13_MOV_LE(VirtualMachine& cpu) noexcept;
	friend void INST0x14_MOV_L(VirtualMachine& cpu) noexcept;
	friend void INST0x15_MOV_E(VirtualMachine& cpu) noexcept;
	friend void INST0x16_MOV_NE(VirtualMachine& cpu) noexcept;
	friend void INST0x17_MOV_C(VirtualMachine& cpu) noexcept;

	friend void INST0x18_MOV_ITOF(VirtualMachine& cpu) noexcept;
	friend void INST0x19_MOV_FTOI(VirtualMachine& cpu) noexcept;

	friend void INST0x1A_FADD(VirtualMachine& cpu) noexcept;
	friend void INST0x1B_FSUB(VirtualMachine& cpu) noexcept;
	friend void INST0x1C_FMUL(VirtualMachine& cpu) noexcept;
	friend void INST0x1D_FDIV(VirtualMachine& cpu) noexcept;
	friend void INST0x1E_FCMP(VirtualMachine& cpu) noexcept;
	friend void INST0x1F_FMOV(VirtualMachine& cpu) noexcept;
	friend void INST0x20_FS(VirtualMachine& cpu) noexcept;
	friend void INST0x21_FL(VirtualMachine& cpu) noexcept;

	friend void INST0x22_SB(VirtualMachine& cpu) noexcept;
	friend void INST0x23_SH(VirtualMachine& cpu) noexcept;
	friend void INST0x24_SW(VirtualMachine& cpu) noexcept;
	friend void INST0x25_LB(VirtualMachine& cpu) noexcept;
	friend void INST0x26_LH(VirtualMachine& cpu) noexcept;
	friend void INST0x27_LW(VirtualMachine& cpu) noexcept;

	friend void INST0x28_JMP(VirtualMachine& cpu) noexcept;
	friend void INST0x29_CALL(VirtualMachine& cpu) noexcept;
	friend void INST0x2A_JGE(VirtualMachine& cpu) noexcept;
	friend void INST0x2B_JG(VirtualMachine& cpu) noexcept;
	friend void INST0x2C_JLE(VirtualMachine& cpu) noexcept;
	friend void INST0x2D_JL(VirtualMachine& cpu) noexcept;
	friend void INST0x2E_JE(VirtualMachine& cpu) noexcept;
	friend void INST0x2F_JNE(VirtualMachine& cpu) noexcept;
	friend void INST0x30_JC(VirtualMachine& cpu) noexcept;

	friend void INST0x31_PUSHB(VirtualMachine& cpu) noexcept;
	friend void INST0x32_POPB(VirtualMachine& cpu) noexcept;

	friend void INST0x33_SWI(VirtualMachine& cpu) noexcept;
	friend void INST0x34_HWI(VirtualMachine& cpu) noexcept;

	// Unplanned addons
	friend void INST0x35_MOD(VirtualMachine& cpu) noexcept;
	friend void INST0x36_MOV_UITOF(VirtualMachine& cpu) noexcept;
	friend void INST0x37_PUSHH(VirtualMachine& cpu) noexcept;
	friend void INST0x38_PUSHW(VirtualMachine& cpu) noexcept;
	friend void INST0x39_POPH(VirtualMachine& cpu) noexcept;
	friend void INST0x3A_POPW(VirtualMachine& cpu) noexcept;
	friend void INST0x3B_PUSHF(VirtualMachine& cpu) noexcept;
	friend void INST0x3C_POPF(VirtualMachine& cpu) noexcept;
	friend void INST0x3D_INC(VirtualMachine& cpu) noexcept;
	friend void INST0x3E_DEC(VirtualMachine& cpu) noexcept;

	friend void INST0xXX_ERR(VirtualMachine& cpu) noexcept;

	// Software interrupt functions.
	friend void handleSWI(VirtualMachine& cpu, int code) noexcept;
	friend void SWI_0x00_TERMINATE(VirtualMachine& cpu) noexcept;
	friend void SWI_0x01_PRINTF(VirtualMachine& cpu) noexcept;
	friend void SWI_0x02_PRINT_INT(VirtualMachine& cpu) noexcept;
	friend void SWI_0x03_PRINT_CHAR(VirtualMachine& cpu) noexcept;
	friend void SWI_0x04_PRINT_FLOAT(VirtualMachine& cpu) noexcept;
	friend void SWI_0x05_PRINT_STRING(VirtualMachine& cpu) noexcept;
	friend void SWI_0x06_READ_STRING(VirtualMachine& cpu) noexcept;
};