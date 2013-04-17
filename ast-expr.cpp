#include "glaz.h"
#include "ast.h"
#include "ast-conv.h"
#include <ostream>

using namespace glaz;

Deref::Deref(Expression *expr) :
        Expression(static_cast<const PointerType *>(
            expr->getType())->getRefType()
        ),
        expr(expr) { }

AddrOf::AddrOf(Expression *expr) :
        Expression(expr->getType()->getPtrType()),
        expr(expr) { }

LitConstant::LitConstant(signed long long ll, const Type *ty) :
        Expression(ty), cached(0), cached_gv(0) {
    
    value.ll = ll;
}

LitConstant::LitConstant(unsigned long long ull, const Type *ty) :
        Expression(ty), cached(0), cached_gv(0) {
    
    value.ull = ull;
}

LitConstant::LitConstant(double d, const Type *ty) :
        Expression(ty), cached(0), cached_gv(0) {
    
    value.d = d;
}

LitConstant::LitConstant(const std::string &str, const Type *ty) :
        Expression(ty), str(str), cached(0), cached_gv(0) {
        
    // Unescape the string (remove quotes and embedded double quotes)
    unsigned i = 1, j = 0;
    for (; i < str.length() - 1; j++) {
        if (str[i] == '"')
            ++i;
        this->str[j] = str[i];
        ++i;
    }
    this->str.resize(j);
}

bool glaz::implicitConvert(Expression *&expr,
        const Type *type, Component *c) {

    if (*expr->getType() == *type)
        return true;
        
    Expression *newexpr = 0;
    
    int tc = type->typeClass();
    int etc = expr->getType()->typeClass();
    
    const IntrinsicType *itfrom =
        static_cast<const IntrinsicType *>(expr->getType());
    const IntrinsicType *itto =
        static_cast<const IntrinsicType *>(type);
        
    switch (tc) {
    case Type::INTRINSIC: {
        if (etc != Type::INTRINSIC)
            break;
            
        int iid = itto->getIntrinsicId();
        int iidfrom = itfrom->getIntrinsicId();
        
        switch (iid) {
        case IntrinsicType::BOOL:
            newexpr = new BinaryCmpOp(BinaryOp::NE, expr,
                new LitConstant(0LL, expr->getType()),
                c->getType("bool")
            );
            break;
            
        // Converting _to_ char from something else.
        case IntrinsicType::CHAR:
        case IntrinsicType::SCHAR:
            switch (iidfrom) {
            case IntrinsicType::BOOL:
                // This will yield 1 for true and 0 for false.
                newexpr = new IntWideningCast(expr, type);
                break;
            
            case IntrinsicType::CHAR:
            case IntrinsicType::SCHAR:
                // We're guaranteed that if they're the same we won't reach
                // this point, so we can assume it's a sign add/remove cast.
                newexpr = new SignCast(expr, type);
                break;
                
            case IntrinsicType::WORD:
            case IntrinsicType::SWORD:
            case IntrinsicType::INT:
            case IntrinsicType::UINT:
            case IntrinsicType::INT64:
            case IntrinsicType::UINT64:
                newexpr = new IntNarrowingCast(expr, type);
                break;
                
            case IntrinsicType::FLOAT:
            case IntrinsicType::DOUBLE:
                newexpr = new FpToIntCast(expr, type);
                break;
            }
            break;
            
        // Converting _to_ word/sword from ___?
        case IntrinsicType::WORD:
        case IntrinsicType::SWORD:
            switch (iidfrom) {
            case IntrinsicType::BOOL:
            case IntrinsicType::CHAR:
            case IntrinsicType::SCHAR:
                newexpr = new IntWideningCast(expr, type);
                break;
                
            case IntrinsicType::WORD:
            case IntrinsicType::SWORD:
                newexpr = new SignCast(expr, type);
                break;
                
            case IntrinsicType::INT:
            case IntrinsicType::UINT:
            case IntrinsicType::INT64:
            case IntrinsicType::UINT64:
                newexpr = new IntNarrowingCast(expr, type);
                break;
                
            case IntrinsicType::FLOAT:
            case IntrinsicType::DOUBLE:
                newexpr = new FpToIntCast(expr, type);
                break;
            }
            break;
            
        // Converting _to_ int/uint from ___?
        case IntrinsicType::INT:
        case IntrinsicType::UINT:
            switch (iidfrom) {
            case IntrinsicType::BOOL:
            case IntrinsicType::CHAR:
            case IntrinsicType::SCHAR:
            case IntrinsicType::WORD:
            case IntrinsicType::SWORD:
                newexpr = new IntWideningCast(expr, type);
                break;

            case IntrinsicType::INT:
            case IntrinsicType::UINT:
                // We're guaranteed that if they're the same we won't reach
                // this point, so we can assume it's a sign add/remove cast.
                newexpr = new SignCast(expr, type);
                break;
                
            case IntrinsicType::INT64:
            case IntrinsicType::UINT64:
                newexpr = new IntNarrowingCast(expr, type);
                break;
                
            case IntrinsicType::FLOAT:
            case IntrinsicType::DOUBLE:
                newexpr = new FpToIntCast(expr, type);
                break;
            }
            break;
            
        // Converting _to_ int64/uint64 from ___?
        case IntrinsicType::INT64:
        case IntrinsicType::UINT64:
            switch (iidfrom) {
            case IntrinsicType::BOOL:
            case IntrinsicType::CHAR:
            case IntrinsicType::SCHAR:
            case IntrinsicType::WORD:
            case IntrinsicType::SWORD:
            case IntrinsicType::INT:
            case IntrinsicType::UINT:
                newexpr = new IntWideningCast(expr, type);
                break;
            
            case IntrinsicType::INT64:
            case IntrinsicType::UINT64:
                // We're guaranteed that if they're the same we won't reach
                // this point, so we can assume it's a sign add/remove cast.
                newexpr = new SignCast(expr, type);
                break;
                
            case IntrinsicType::FLOAT:
            case IntrinsicType::DOUBLE:
                newexpr = new FpToIntCast(expr, type);
                break;
            }
            break;
            
        default:
            assert(0 && "can't yet convert an int to an fp type.");
        }
    }
        
        break;
        
    case Type::POINTER:
        if (etc == Type::ARRAY) {
            // If it's an array, it must be the same underlying type
            // or a void*.
            if (!(*static_cast<const PointerType *>(type)->getRefType() ==
                    *static_cast<const ArrayType *>(expr->getType())
                        ->getRefType() ||
                    static_cast<const PointerType *>(type)
                        ->getRefType()->isVoid()))
                return false;
        } else if (etc != Type::POINTER) {
            return false;
        }
            
        newexpr = new PointerCast(expr, type);
            
        break;
    }
    
    if (newexpr) {
        expr = newexpr;
        return true;
    }
    
    return false;
}

bool glaz::implicitConvert(Expression *&left, Expression *&right,
        Component *c) {
    
    if (*left->getType() == *right->getType())
        return true;
        
    return false;
}

CallExpr::CallExpr(Sub *sub) :
    Expression(static_cast<const SubType *>(
        sub->getType())->getReturnType()
    ), sub(sub),
    subty(static_cast<const SubType *>(sub->getType())), expridx(0) { }

CallExpr::CallExpr(const SubType *subty) :
    Expression(subty->getReturnType()), sub(0), subty(subty), expridx(0) { }

bool CallExpr::pushExpr(Expression *expr,
        Component *c) {
    
    if (expridx >= subty->paramSize()) {
        if (subty->getFlags() & SubType::VARARGS) {
            exprlist.push_back(expr);
            ++expridx;
            return true;
        }
        
        // Otherwise not varargs, we have too many params.
        return false;
    }
    
    if (!implicitConvert(expr, subty->getParam(expridx), c))
        return false;
        
    exprlist.push_back(expr);
    ++expridx;
    return true;
}

bool CallExpr::hasEnoughParams() const {
    return expridx >= subty->paramSize();
}

