#include "glaz.h"
#include "ast.h"

using namespace glaz;

namespace glaz {
    namespace xml {
        std::ostream &indented(std::ostream &file, int indent) {
            for (int i = 0; i < indent; ++i) {
                file << ' ';
            }
            return file;
        }
    }
}

using glaz::xml::indented;

void Component::toXml(std::ostream &file) const {
    file << "<?xml version=\"1.0\" ?>\n";
    file << "<Component name=\"Unknown\">\n";
    
    for (type_map::const_iterator it = types.begin(); it != types.end(); ++it) {
        it->second->toXml(file, 1);
    }
        
    for (var_map::const_iterator it = vars.begin(); it != vars.end(); ++it) {
        it->second->toXml(file, 1);
    }
        
    file << "</Component>\n";
}

void IntrinsicType::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<IntrinsicType name=\"" << name <<
        "\" id=\"" << which << "\" />\n";
}

void Struct::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<Struct name=\"" << name <<
        "\" align=\"";
    if (alignment == 0)
        file << "default\">\n";
    else
        file << alignment << "\">\n";
    
    for (var_list::const_iterator it = vars.begin(); it != vars.end(); ++it) {
        it->second->toXml(file, indent + 1);
    }
    indented(file, indent) << "</Struct>\n";
}

void Var::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<Var>\n";
    indented(file, indent+1) << "<Type>" << type->getName() << "</Type>\n";
    indented(file, indent+1) << "<Name>" << name << "</Name>\n";
    if (initializer) {
        indented(file, indent+1) << "<Initial>\n";
        initializer->toXml(file, indent+2);
        indented(file, indent+1) << "</Initial>\n";
    }
    indented(file, indent) << "</Var>\n";
}

void Label::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<Label name=\"" << name << "\" />\n";
}

void Sub::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<Sub name=\"" << getName() <<
        "\" implemented=\"" << ((flags & IMPLEMENTED) ? "true" : "false") <<
        "\"";
    //if (flags & CDECL)
    //  file << " callconv=\"cdecl\"";
    if (flags & COMMAND)
        file << " command=\"true\"";
    file << ">\n";
    
    if (flags & HAS_ALIAS)
        indented(file, indent+1) << "<Alias>" << aliasname << "</Alias>\n";
    
    if (flags & HAS_LIB)
        indented(file, indent+1) << "<Lib>" << libname << "</Lib>\n";
    
    for (var_map::const_iterator it = vars.begin(); it != vars.end(); ++it) {
        it->second->toXml(file, indent+1);
    }
    file << "\n";
    
    for (const InOrderNode *node = first; node; node = node->next) {
        node->toXml(file, indent+1);
    }
    
    indented(file, indent) << "</Sub>\n";
}

void Goto::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<Goto>" << label->getName() << "</Goto>\n";
}

void PointerType::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<PointerType>" << referenced->getName() <<
        "</PointerType>\n";
}

void SubType::toXml(std::ostream &file, int indent) const {
    indented(file, indent) << "<SubType>" << getName() << "</SubType>\n";
}

void LitConstant::toXml(std::ostream &file, int indent) const {
    xml::indented(file, indent) << "<LitConstant>\n";
    xml::indented(file, indent+1) << "<Type>" << type->getName() << "</Type>\n";
    xml::indented(file, indent+1) << "<Value>";
    
    if (type->typeClass() == Type::POINTER) {
        // The only pointer literals are strings.
        file << str;
    } else {
        IntrinsicType *ty = static_cast<IntrinsicType*>(type.get());
        switch (ty->getIntrinsicId()) {
        case IntrinsicType::INT:
            file << value.i;
            break;
            
        case IntrinsicType::UINT:
            file << value.u;
            break;
            
        case IntrinsicType::INT64:
            file << value.ll;
            break;
            
        case IntrinsicType::UINT64:
            file << value.ull;
            break;
            
        case IntrinsicType::FLOAT:
            file << value.f;
            break;
            
        case IntrinsicType::DOUBLE:
            file << value.d;
            break;
            
        default:
            assert(0 && "invalid type value for literal constant");
        }
    }
    
    file << "</Value>\n";
    xml::indented(file, indent) << "</LitConstant>\n";
}

/*void VarRef::toXml(std::ostream &file, int indent) const {
    xml::indented(file, indent) << "<VarRef>" << var->getName() <<
        "</VarRef>\n";
}*/

void Assign::toXml(std::ostream &file, int indent) const {
    xml::indented(file, indent) << "<Assign>\n";
    xml::indented(file, indent+1) << "<Lvalue>\n";
    left->toXml(file, indent+2);
    xml::indented(file, indent+1) << "</Lvalue>\n";
    xml::indented(file, indent+1) << "<Rvalue>\n";
    right->toXml(file, indent+2);
    xml::indented(file, indent+1) << "</Rvalue>\n";
    xml::indented(file, indent) << "</Assign>\n";
}

namespace {
    const char *const unary_ops[] = {
        "INVALID",
        "-",
        "NOT",
    };
    
    const char *const binary_ops[] = {
        "INVALID",
        "+", "-",
        "*", "/", "%", "^",
        "and", "or", "xor",
        "shr", "shl",
        "lt", "gt", "=", "le", "ge", "!=",
    };
}

void UnaryOp::toXml(std::ostream &file, int indent) const {
    xml::indented(file, indent) << "<UnaryOperator op=\"" <<
        unary_ops[op] << "\">\n";
    child->toXml(file, indent+1);
    xml::indented(file, indent) << "</UnaryOperator>\n";
}

void BinaryOp::toXml(std::ostream &file, int indent) const {
    xml::indented(file, indent) << "<BinaryOperator op=\"" <<
        binary_ops[op] << "\">\n";
    xml::indented(file, indent+1) << "<Left>\n";
    left->toXml(file, indent+2);
    xml::indented(file, indent+1) << "</Left>\n";
    xml::indented(file, indent+1) << "<Right>\n";
    right->toXml(file, indent+2);
    xml::indented(file, indent+1) << "</Right>\n";
    xml::indented(file, indent) << "</BinaryOperator>\n";
}


