#pragma once

class Compiler;

#include "Types.h"

#define USABLE_REGISTER_START R2

enum class QueSymbolAccessType {
	UNDEFINED,
	LOCAL,
	IMPORTED,
	EXPORTED,
	ARGUMENT,
	VARIABLE,
	COMPILER_ACCESS
};

enum class QueSymbolType {
	UNDEFINED,
	DATA,
	FUNCTION,
	CONST_EXPR,
	ARRAY
};

enum class RegisterAllocMode {
	DONT_LOAD,
	LOAD_ADDRESS_OR_VALUE,
	LOAD_VALUE
};

struct SymbolInfo {
	~SymbolInfo() {
		for (int i = 0; i < fnArgs.size(); i++) {
			delete fnArgs[i];
		}
	}

	SymbolInfo(int stackFrameIndex) 
		:stackFrameIndex(stackFrameIndex)
	{
	}

	QueSymbolAccessType accessType = QueSymbolAccessType::UNDEFINED;
	QueSymbolType symbolType = QueSymbolType::UNDEFINED;
	TypeInfo typeInfo;
	int value = 0;
	int temporaryRegisterBPOffset = -1;
	bool isGlobal = false;
	bool isTemporaryValue = false;
	int stackFrameIndex = 0;
	std::string name;

	// Temporary symbols have a different name, but sometimes the name
	// of their derivative symbols needs to be referenced. This refers to the name of the
	// actual symbol in the file.
	std::string fileName;

	// For array types, they have n dimensions, and n sizes per dimension
	std::vector<int> dimensions;
	std::vector<int> totalDimensionSizes;
	int totalElementCount;
	
	// only applicable to function symbols.
	// Contains a list of all information about its arguments.
	std::vector<SymbolInfo*> fnArgs;
	// Stores a list of places to jump to when escaping stackframes.
	int baseStackFrameSize = 0;
	int functionReturnPointer = 0;
	std::vector<std::vector<int>> functionReturnPointers; 
	std::vector<std::vector<int>> functionStackFrameSizes; 
	// Used for functions, says whether the symbol has been implemented or not in this assembly.
	// Used for globals if the variable has been initialized (in pass2)
	bool defined = false;

	void calculateTotalDimensionElements() {
		int previousDimensionSize = 1;
		totalDimensionSizes.resize(dimensions.size());

		for (int i = dimensions.size() - 1; i >= 0; i--) {
			previousDimensionSize = previousDimensionSize * dimensions[i];
			totalDimensionSizes[i] = previousDimensionSize;
		}
	}

	int calculateTotalSize() {
		uint64_t totalElements = 1;
		for (int i = this->dimensions.size() - 1; i >= 0; i--) {
			totalElements *= this->dimensions[i];
		}

		uint64_t size = this->typeInfo.sizeInMemory();

		size *= totalElements;
		return size;
	}
};

/// <summary>
/// Local symbol tables don't have to worry about things like imported
/// and exported symbols.
/// </summary>
class LocalSymbolTable {
public:
	LocalSymbolTable()
	{
		clear();
		// This table is referring to local variables. 
		// Based on this calling convention, when we enter a new stack frame,
		// the base pointer comes right before RA, meaning its offset
		// by 4 bytes from the first stack push.
		// When adding a variable to the stack frame,
		// subtract the size first, then copy the value in at the current SP
		currentOffsetFromBP = 0;

		for (std::map<std::string, SymbolInfo*>::iterator i = symbolInfo.begin(); i != symbolInfo.end(); i++) {
			delete i->second;
		}
	}

	void clear() {
		symbolInfo.clear();
		currentOffsetFromBP = 0;
	}

	bool symbolExists(const std::string& name) {
		std::map<std::string, SymbolInfo*>::iterator foundSymbol =
			symbolInfo.find(name);

		return foundSymbol != symbolInfo.end();
	}

	bool addSymbol(const std::string& symbol, SymbolInfo* info) {
		if (symbolInfo.find(symbol) == symbolInfo.end()) {
			info->isGlobal = false;
			symbolInfo[symbol] = info;
			return true;
		}
		return false;
	}

	SymbolInfo* getSymbolInfo(const std::string& symbol) {
		std::map<std::string, SymbolInfo*>::iterator foundSymbol = symbolInfo.find(symbol);

		if (foundSymbol != symbolInfo.end()) {
			return symbolInfo[symbol];
		}

		return nullptr;
	}

	/// <summary>
	/// Size should be a multiple of 4 to stay on a word boundary.
	/// </summary>
	/// <param name="compiler"></param>
	/// <param name="size"></param>
	void pushLocalVariable(Compiler* compiler, int size);

	int getLocalBPOffset() {
		return currentOffsetFromBP;
	}

	// Maps a symbol name to some info about it.
	std::map<std::string, SymbolInfo*> symbolInfo;
	int currentOffsetFromBP = 0;
	int totalFrameSize = 0;
};

class GlobalSymbolTable {
public:
	GlobalSymbolTable() {
		clear();
	}

	void clear() {
		globalOffsetTable.clear();
		exportedSymbolCount = 0;
		symbolInfo.clear();
		symbolImportReferencesDataSeg.clear();
		symbolImportReferencesTextSeg.clear();
	}

	bool addSymbol(const std::string& name, SymbolInfo*& info) {
		std::map<std::string, SymbolInfo*>::iterator foundSymbol =
			symbolInfo.find(name);

		info->isGlobal = true;

		if (foundSymbol == symbolInfo.end()) {
			symbolInfo.emplace(name, info);

			if (info->accessType == QueSymbolAccessType::EXPORTED) {
				exportedSymbolCount++;
			}

			return true;
		}
		else {
			// Preserve the variables you want to carry over between rounds
			SymbolInfo* previous = info;
			int previousFunctionReturn = symbolInfo[name]->functionReturnPointer;
			int previousStackFrameSize = symbolInfo[name]->baseStackFrameSize;
			std::vector<std::vector<int>> previousFnReturnPtrs = symbolInfo[name]->functionReturnPointers;
			std::vector<std::vector<int>> previousFrameSizes = symbolInfo[name]->functionStackFrameSizes;

			*symbolInfo[name] = *previous;
			info = symbolInfo[name];
			info->functionReturnPointer = previousFunctionReturn;
			info->functionReturnPointers = previousFnReturnPtrs;
			info->functionStackFrameSizes = previousFrameSizes;
			info->baseStackFrameSize = previousStackFrameSize;
			delete previous;
		}

		return false;
	}

	bool symbolExists(const std::string& name) {
		std::map<std::string, SymbolInfo*>::iterator foundSymbol =
			symbolInfo.find(name);

		return foundSymbol != symbolInfo.end();
	}

	SymbolInfo* getSymbolInfo(const std::string& name) {
		std::map<std::string, SymbolInfo*>::iterator foundSymbol =
			symbolInfo.find(name);

		if (foundSymbol != symbolInfo.end()) {
			return symbolInfo[name];
		}

		return nullptr;
	}

	void addImportReferenceTextSeg(const std::string& symbol, int binaryOffset) {
		if (symbolImportReferencesTextSeg.find(symbol) == symbolImportReferencesTextSeg.end()) {
			symbolImportReferencesTextSeg.emplace(symbol, std::vector<int>());
		}

		symbolImportReferencesTextSeg[symbol].push_back(binaryOffset);
	}

	void addImportReferenceDataSeg(const std::string& symbol, int binaryOffset) {
		if (symbolImportReferencesDataSeg.find(symbol) == symbolImportReferencesDataSeg.end()) {
			symbolImportReferencesDataSeg.emplace(symbol, std::vector<int>());
		}

		symbolImportReferencesDataSeg[symbol].push_back(binaryOffset);
	}

	void addLocalDataSegmentReference(int binaryOffset) {
		globalOffsetTable.push_back(binaryOffset);
	}

	std::map<std::string, SymbolInfo*> symbolInfo;
	std::map<std::string, std::vector<int>> symbolImportReferencesTextSeg;
	std::map<std::string, std::vector<int>> symbolImportReferencesDataSeg;
	int exportedSymbolCount;
	std::vector<int> globalOffsetTable;
};

/// <summary>
/// Holds information about a symbol scope.
/// </summary>
class SymbolStack {
public:
	const int OFFSET_BETWEEN_FRAMES = 8;

	SymbolStack(Compiler* compiler) {
		this->compiler = compiler;
	}

	~SymbolStack() {
		// Delete each ptr.
		for (int i = 0; i < localScope.size(); i++) {
			delete localScope[i];
		}
	}

	int getCurrentBPOffset() {
		assert(!atGlobalScope());
		return localScope[scopeIndex]->getLocalBPOffset();
	}

	void setFrameSize(int offset) {
		assert(!atGlobalScope());
		localScope[scopeIndex]->totalFrameSize = offset;
	}

	int calculateBPOffsetFromCurrentStackFrame(SymbolInfo& symbol) {
		assert(!atGlobalScope());
		int offset = symbol.value;
		// The base ptr lies at the very bottom of the current scope.
		// Loop from zero to the symbol's frame index and count up the total offset.

		for (int i = 0; i < symbol.stackFrameIndex; i++) {
			offset += localScope[i]->totalFrameSize;
			//offset -= OFFSET_BETWEEN_FRAMES;
		}

		return -offset;
	}

	bool variableExists(const std::string& symbolName) {
		for (int i = 0; i < localScope.size(); i++) {
			if (localScope[i]->symbolExists(symbolName)) {
				return true;
			}
		}

		if (globalSymbols.symbolExists(symbolName)) {
			return true;
		}

		return false;
	}

	SymbolInfo* searchSymbol(const std::string& symbol) {
		SymbolInfo* ret;
		for (int i = localScope.size() - 1; i >= 0; i--) {
			ret = localScope[i]->getSymbolInfo(symbol);
			if (ret) {
				return ret;
			}
		}

		// If we cant find it in any local scope, search for it in the global scope.
		ret = globalSymbols.getSymbolInfo(symbol);

		if (ret) {
			return ret;
		}
		
		// If we are on the first pass, not having symbol information isn't a problem.
		return nullptr;
	}

	bool addSymbolToCurrentScope(SymbolInfo*& info) {
		if (scopeIndex >= 0) {
			// Add to a local scope
			info->stackFrameIndex = scopeIndex;

			if (!localScope[scopeIndex]->addSymbol(info->name, info)) {
				//compiler->addError("Symbol defined multiple times in this scope: " + info->name);
				return false;
			}
		}
		else {
			// Add it to the global scope
			if (!globalSymbols.addSymbol(info->name, info)) {
				return false;
			}
		}

		return true;
	}

	/// <summary>
	/// Pushes a new scope.
	/// </summary>
	void pushScope() {
		// If we are starting a new function stack, create a new register 
		// allocation frame.
		if (scopeIndex == 0) {
			// R0 is reserved for the return address.
			// R1 is reserved for storing the address to store globals after free.

			for (int i = R15; i >= USABLE_REGISTER_START; i--) {
				availableRegisters.push((Register)i);
			}
		}

		scopeIndex++;
		if (scopeIndex >= localScope.size()) {
			localScope.push_back(new LocalSymbolTable());
		}
	}

	/// <summary>
	/// Removes the previous scope from the list. Will not remove the global stack.
	/// </summary>
	bool popScope() {
		// If we go back to the zero's scope, we have to reset the register
		// allocation stack because each function assumes a blank register state.
		if (scopeIndex == 0) {
			assert(registerInUse.size() == 0);

			registerAllocationStack.clear();
			availableRegisters = std::stack<Register>();
			registerInUse.clear();
		}

		if (scopeIndex >= 0) {
			localScope[scopeIndex]->clear();
			scopeIndex--;
		}
		else {
			return false;
		}

		return true;
	}

	/// <summary>
	/// Sends us to the global scope, any identifiers written during this time will
	/// be written at the global level.
	/// </summary>
	void enterPreviousScope() {
		if (!atGlobalScope()) {
			if (saveScopeIndex == -1) {
				saveScopeIndex = scopeIndex;
			}

			scopeIndex--;
		}
	}

	/// <summary>
	/// If the scope is not pointing to the top, we will reenter it.
	/// </summary>
	void enterCurrentScope() {
		if (saveScopeIndex != -1) {
			scopeIndex = saveScopeIndex;
			saveScopeIndex = -1;
		}
	}

	bool atGlobalScope() {
		return scopeIndex < 0;
	}

	void pushVariable(SymbolInfo& info);

	// Should be called right before a register is used.
	int allocateIntRegister(SymbolInfo& variable, RegisterAllocMode loadMode);
	void freeIntRegister(SymbolInfo& registerToFree, bool preserveValue);

	// Information used for register allocation.
	std::map<std::string, Register> registerAllocationStack;
	std::vector<Register> registerInUse;
	std::stack<Register> availableRegisters;

	int saveScopeIndex = -1;
	std::vector<LocalSymbolTable*> localScope;
	GlobalSymbolTable globalSymbols;
	Compiler* compiler;
	int scopeIndex = -1;

	long getTemporaryVariableIndex() {
		return temporaryVariableIdx++;
	}

	long temporaryVariableIdx = 0;
};

