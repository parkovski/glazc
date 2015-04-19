#ifndef AST_EXPR_H
#define AST_EXPR_H

#include <cassert>

namespace glaz {

class Type;

class Expression {
protected:
    const Type *type;
    
public:
    enum {
        LITCONST,
        SUB,
        GLOBALVAR,
        LOCALVAR,
        PARAM,
        UNARYOP,
        BINARYOP,
        CALLEXPR,
        CONVERSION,
        DEREF,
        ADDROF,
        ARRAYINDEX,
        STRUCTACCESS
    };
    
    explicit Expression(const Type *ty) : type(ty) { }
    virtual ~Expression() { }
    
    const Type *getType() const { return type; }
    virtual int exprClass() const = 0;
    
    // If isConst returns true, then fold is required to return a constant
    // expression of the same type.
    virtual bool isConst() { return false; }
    virtual Expression *fold() { return 0; }
};

} // namespace glaz

#endif        //  #ifndef AST_EXPR_H

