
#include "Compiler.h"

void Compiler::writeAssignment(SymbolInfo& left, SymbolInfo& right) {
	// Assign a register to the right symbol, then assign one to the right symbol
	// If we are an array or function, storing the address is fine, otherwise, the register should
	// hold the value.
	if (left.symbolType == QueSymbolType::ARRAY ||
		left.symbolType == QueSymbolType::FUNCTION ||
		left.symbolType == QueSymbolType::CONST_EXPR ||
		left.symbolType == QueSymbolType::UNDEFINED ||
		left.accessType == QueSymbolAccessType::COMPILER_ACCESS ||
		left.isTemporaryValue)
	{
		addError("L value expected in assignment");
	}
	else {
		int leftRegister = symbolStack.allocateIntRegister(left, RegisterAllocMode::DONT_LOAD);
		int rightRegister = symbolStack.allocateIntRegister(right, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);

		// The right register now includes the value to write to the left register's
		// address

		int offset = writeLoadAddressInstructions(left, leftRegister);
		if (offset == 0) {
			writeStoreValueInAddressInstructions(left, 0, leftRegister, rightRegister);
		}
		else {
			writeStoreValueInAddressInstructions(left, offset, BP, rightRegister);
		}

		// Free register usage
		symbolStack.freeIntRegister(right, false);
		symbolStack.freeIntRegister(left, false);
	}
}

void Compiler::writeOperatorInstructions(SymbolInfo& left, SymbolInfo& right, Operator op) {
	// Check to see if we have constants so we can optimize that if necessasry
	/** TODO left and right can also be constant identifiers
	if (left.isTemporaryValue && right.isTemporaryValue) {
		ConstantValue leftConst;
		ConstantValue rightConst;

		
		return;
	}
	*/

	int leftRegister = symbolStack.allocateIntRegister(left, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);
	int rightRegister = symbolStack.allocateIntRegister(right, RegisterAllocMode::LOAD_ADDRESS_OR_VALUE);

	uint32_t opcode = 0;

	if (op == Operator::ADD) {
		opcode = encodeInstruction(ADD, false, leftRegister, leftRegister, rightRegister);
	}
	else if (op == Operator::SUB) {
		opcode = encodeInstruction(SUB, false, leftRegister, leftRegister, rightRegister);
	}
	else if (op == Operator::MULT) {
		opcode = encodeInstruction(MUL, false, leftRegister, leftRegister, rightRegister);
	}
	else if (op == Operator::DIV) {
		opcode = encodeInstruction(DIV, false, leftRegister, leftRegister, rightRegister);
	}
	else if (op == Operator::MOD) {
		opcode = encodeInstruction(MOD, false, leftRegister, leftRegister, rightRegister);
	}

	writeInstruction(opcode);
}

void Compiler::writeLoadValueOrAddressInstructions(SymbolInfo& value, int registerToUse) {
	if (value.symbolType == QueSymbolType::ARRAY || value.symbolType == QueSymbolType::FUNCTION) {
		writeLoadAddressInstructions(value, registerToUse);
	}
	else {
		writeLoadValueInstructions(value, registerToUse);
	}
}

void Compiler::writeLoadValueInstructions(SymbolInfo& info, int registerToUse) {
	if (info.isGlobal) {
		writeLoadAddressInstructions(info, registerToUse);
		writeLoadValueFromAddressInstructions(info, 0, registerToUse, registerToUse);
	}
	else if (info.isTemporaryValue) {
		writeInstruction(encodeInstruction(MOV, true, registerToUse), true, info.value);
	}
	else {
		int offsetFromBP = symbolStack.calculateBPOffsetFromCurrentStackFrame(info);
		writeLoadValueFromAddressInstructions(info, offsetFromBP, BP, registerToUse);
	}
}

int Compiler::writeLoadAddressInstructions(SymbolInfo& info, int registerToUse) {
	// The location for the variable is stored as a raw value, but we need to calculate the offset from 
	// the LA instruction
	if (info.isGlobal) {
		if (info.accessType == QueSymbolAccessType::EXPORTED || info.accessType == QueSymbolAccessType::LOCAL) {
			int rel = info.value - (currentBinaryOffset + 4ULL);
			writeInstruction(encodeInstruction(LA, true, registerToUse), true, rel);
		}
		else {
			// TODO handle imports
			symbolStack.globalSymbols.addImportReferenceTextSeg(info.name, currentBinaryOffset + 4ULL);
			writeInstruction(encodeInstruction(LA, true, registerToUse), true, 0);
		}
	}
	else {
		int offsetFromBP = symbolStack.calculateBPOffsetFromCurrentStackFrame(info);
		// writeInstruction(encodeInstruction(ADD, true, registerToUse, BP), true, offsetFromBP);
		return offsetFromBP;
	}

	return 0;
}

void Compiler::writeStoreValueInAddressInstructions(SymbolInfo& info, int offset, int addressReg, int valueReg) {
	bool immediate = false;
	if (offset != 0) {
		immediate = true;
	}

	if (info.typeInfo.sizeInMemory() == 1) {
		writeInstruction(encodeInstruction(SB, immediate, valueReg, addressReg), immediate, offset);
	}
	else if (info.typeInfo.sizeInMemory() == 2) {
		writeInstruction(encodeInstruction(SH, immediate, valueReg, addressReg), immediate, offset);
	}
	else if (info.typeInfo.sizeInMemory() == 4) {
		writeInstruction(encodeInstruction(SW, immediate, valueReg, addressReg), immediate, offset);
	}
}

void Compiler::writeLoadValueFromAddressInstructions(SymbolInfo& info, int offset, int addressReg, int destReg) {
	bool immediate = false;
	if (offset != 0) {
		immediate = true;
	}

	if (info.typeInfo.sizeInMemory() == 1) {
		writeInstruction(encodeInstruction(LB, immediate, destReg, addressReg), immediate, offset);
	}
	else if (info.typeInfo.sizeInMemory() == 2) {
		writeInstruction(encodeInstruction(LH, immediate, destReg, addressReg), immediate, offset);
	}
	else if (info.typeInfo.sizeInMemory() == 4) {
		writeInstruction(encodeInstruction(LW, immediate, destReg, addressReg), immediate, offset);
	}
}