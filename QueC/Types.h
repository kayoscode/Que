#pragma once

enum class TypeCode {
	TYPE_INT8_CODE,
	TYPE_INT16_CODE,
	TYPE_INT32_CODE,
	TYPE_BOOL_CODE,
	TYPE_FLOAT32_CODE,
	CONSTANT_TYPE_COUNT,
	UNDEFINED,
};

enum Register {
	R0, R1, R2, R3,
	R4, R5, R6, R7,
	R8, R9, R10, R11,
	R12, R13, R14, R15,
	PC, BP, SP, FR, RA,
	INT_REGISTER_COUNT
};

enum ModifierFlags {
	EXPORT_FLAG = 1,
	EXTERN_FLAG = 2,
	ARGUMENT_FLAG = 4,
	ENTRYPT_FLAG = 8,
	LOCAL_VAR_FLAG = 16,
};

enum class Operator {
	MULT,
	DIV,
	ADD,
	SUB,
	MOD,
	AND,
	OR,
	XOR,
	LSL,
	LSR,

	CMP_GT,
	CMP_LT,
	CMP_GTE,
	CMP_LTE,
	CMP_EQ,
	CMP_NE,

	// Logical ops
	AND_AND,
	OR_OR,
	NOT,

	ARRAY_INDEX,
};

/// <summary>
/// Includes all information attached to a typed symbol.
/// </summary>
struct TypeInfo {
	int baseSize = 0;
	TypeCode typeCode = TypeCode::UNDEFINED;
	int ptrCount = 0;

	int sizeInMemory() {
		if (ptrCount > 0) {
			return 4;
		}

		return baseSize;
	}
};

/// <summary>
/// Data defined for a type
/// </summary>
struct TypeDef {
	TypeCode code = TypeCode::UNDEFINED;
	int size = -1;
};

/// <summary>
/// Table storing types and their various sizes.
/// Soon we will have to deal with a type stack, but we can handle that later.
/// </summary>
class TypeTable {
public:
	TypeTable() :
		typeData() 
	{
	}

	TypeCode addType(const std::string& typeName, int size, TypeCode code) {
		TypeDef def;
		def.code = code;
		def.size = size;
		typeCodes.emplace(typeName, code);
		typeData.emplace(code, def);
		return code;
	}

	bool typeExists(const std::string& typeName) {
		return typeCodes.find(typeName) != typeCodes.end();
	}

	bool typeExists(TypeCode code) {
		return typeData.find(code) != typeData.end();
	}

	int getTypeSize(const std::string& typeName) {
		return typeData[typeCodes[typeName]].size;
	}

	TypeCode getTypeCode(const std::string& typeName) {
		return typeCodes[typeName];
	}

	int getTypeSize(TypeCode code) {
		return typeData[code].size;
	}

private:
	std::map<TypeCode, TypeDef> typeData;
	std::map<std::string, TypeCode> typeCodes;
};

