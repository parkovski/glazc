#include "ast.h"
#include "ast-conv.h"

#include <sstream>

using namespace glaz;

std::string Component::toString() const {
    std::stringstream ss;
    ss << "component:\n";
    ss << "types:\n";
    for (auto const typePair : types) {
        ss << typePair.second->toString() << "\n";
    }
    ss << "vars:\n";
    for (auto const varPair : vars) {
        ss << varPair.second->toString() << "\n";
    }
    return ss.str();
}

// Types

std::string IntrinsicType::toString() const {
    std::stringstream ss;
    ss << "intrinsic type ";
    switch (getIntrinsicId()) {
    case VOID:
        ss << "void";
        break;
    case BOOL:
        ss << "bool";
        break;
    case CHAR:
        ss << "char";
        break;
    case SCHAR:
        ss << "schar";
        break;
    case WORD:
        ss << "word";
        break;
    case SWORD:
        ss << "sword";
        break;
    case INT:
        ss << "int";
        break;
    case UINT:
        ss << "uint";
        break;
    case INT64:
        ss << "int64";
        break;
    case UINT64:
        ss << "uint64";
        break;
    case FLOAT:
        ss << "float";
        break;
    case DOUBLE:
        ss << "double";
        break;
    default:
        ss << "(unknown)";
        break;
    }
    ss << ";";
    return ss.str();
}

std::string Struct::toString() const {
    return "struct " + name + ";";
}

std::string PointerType::toString() const {
    return "pointer (" + referenced->getName() + ");";
}

std::string ArrayType::toString() const {
    return "array (" + referenced->getName() + ");";
}

std::string SubType::toString() const {
    return "sub type " + this->getName() + ";";
}

// Expressions

std::string UnaryOp::toString() const {
    return "";
}

std::string BinaryOp::toString() const {
    return "";
}

std::string Deref::toString() const {
    return "";
}

std::string AddrOf::toString() const {
    return "";
}

std::string LitConstant::toString() const {
    auto intrinsicId = static_cast<const IntrinsicType *>(type)->getIntrinsicId();

    std::stringstream ss;

    switch (intrinsicId) {
    case IntrinsicType::BOOL:
        ss << value.b ? "true" : "false";
        break;
    case IntrinsicType::INT:
        ss << value.i;
        break;
    case IntrinsicType::UINT:
        ss << value.u;
        break;
    case IntrinsicType::INT64:
        ss << value.ll;
        break;
    case IntrinsicType::UINT64:
        ss << value.ull;
        break;
    case IntrinsicType::FLOAT:
        ss << value.f;
        break;
    case IntrinsicType::DOUBLE:
        ss << value.d;
        break;
    default:
        // char array (string)
        assert(type->typeClass() == Type::ARRAY);
        ss << this->str;
    }
    return ss.str();
}

// subtypes: LocalVar, GlobalVar, Param
std::string Var::toString() const {
    return "var " + this->name + ";";
}

std::string ArrayIndexer::toString() const {
    return "";
}

std::string StructAccessor::toString() const {
    return "";
}

std::string CallExpr::toString() const {
    return "";
}

std::string PtrCallExpr::toString() const {
    return "";
}

// Cast operators defined in ast-conv.h
std::string PointerCast::toString() const {
    return "";
}

std::string SignCast::toString() const {
    return "";
}

std::string IntWideningCast::toString() const {
    return "";
}

std::string IntNarrowingCast::toString() const {
    return "";
}

std::string IntToFpCast::toString() const {
    return "";
}

std::string FpToIntCast::toString() const {
    return "";
}

// InOrderNodes
std::string Label::toString() const {
    return "";
}

std::string CallStmt::toString() const {
    return "";
}

std::string Goto::toString() const {
    return "";
}

std::string Assign::toString() const {
    return "";
}

std::string Return::toString() const {
    return "";
}

// Containers (all except Sub are also InOrderNodes)
std::string IfBlock::toString() const {
    return "";
}

std::string SelectBlock::toString() const {
    return "";
}

std::string CaseBlock::toString() const {
    return "";
}

std::string ForBlock::toString() const {
    return "";
}

std::string WhileBlock::toString() const {
    return "";
}

std::string DoBlock::toString() const {
    return "";
}

std::string Sub::toString() const {
    return "sub " + this->getName() + ";";
}