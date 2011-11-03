#ifndef AST_EXPR_H
#define AST_EXPR_H

#include <cassert>

namespace llvm {
    class LLVMContext;
    class Module;
    class BasicBlock;
    class Value;
}

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
    //virtual void toXml(std::ostream &file, int indent) const { }
    virtual int exprClass() const = 0;
    
    // If isConst returns true, then fold is required to return a constant
    // expression of the same type.
    virtual bool isConst() { return false; }
    virtual Expression *fold() { return 0; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const = 0;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
            llvm::Module &mod, llvm::BasicBlock *block) const {
        
        assert(0 && "can't take address of unknown expression type");
        return 0;
    }
};

} // namespace glaz

#endif        //  #ifndef AST_EXPR_H

