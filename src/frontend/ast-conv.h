#ifndef AST_CONV_H
#define AST_CONV_H

#include "ast-expr.h"

#include <cassert>

namespace glaz {

class Type;

// Used to cast between pointers and arrays of different types. Does a simple
// llvm bitcast.
class PointerCast : public Expression {
    Expression *expr;
    
public:
    explicit PointerCast(Expression *expr, const Type *convto) :
        Expression(convto), expr(expr) { }
    
    virtual int exprClass() const { return CONVERSION; }

    virtual std::string toString() const override;
};

// Used to add or remove the sign to an integral type. Is essentially a no-op,
// as getLlvmValue is forwarded to the inner expression, just the type is
// replaced.
class SignCast : public Expression {
    Expression *expr;
    
public:
    explicit SignCast(Expression *expr, const Type *convto) :
        Expression(convto), expr(expr) { }
    
    virtual int exprClass() const { return CONVERSION; }

    virtual std::string toString() const override;
};

// Used to implement either a sign-extend or zero-extend operation, depending
// on the type of the inner expression.
class IntWideningCast : public Expression {
    Expression *expr;
    
public:
    explicit IntWideningCast(Expression *expr, const Type *convto) :
        Expression(convto), expr(expr) { }
        
    virtual int exprClass() const { return CONVERSION; }

    virtual std::string toString() const override;
};

// Truncates the value of the inner expression. Results could be undesirable
// if the original value doesn't fit in the new type.
class IntNarrowingCast : public Expression {
    Expression *expr;
    
public:
    explicit IntNarrowingCast(Expression *expr, const Type *convto) :
        Expression(convto), expr(expr) { }
        
    virtual int exprClass() const { return CONVERSION; }

    virtual std::string toString() const override;
};

// Converts a signed or unsigned integer to floating point.
class IntToFpCast : public Expression {
    Expression *expr;
    
public:
    explicit IntToFpCast(Expression *expr, const Type *convto) :
        Expression(convto), expr(expr) { }
        
    virtual int exprClass() const { return CONVERSION; }

    virtual std::string toString() const override;
};

// Converts a floating point number to a signed or unsigned integer. Loss of
// data is possible and should be expected.
class FpToIntCast : public Expression {
    Expression *expr;
    
public:
    explicit FpToIntCast(Expression *expr, const Type *convto) :
        Expression(convto), expr(expr) { }
        
    virtual int exprClass() const { return CONVERSION; }

    virtual std::string toString() const override;
};

} // namespace glaz

#endif        //  #ifndef AST_CONV_H

