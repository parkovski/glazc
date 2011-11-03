#include "glaz.h"
#include "ast.h"
#include "ast-conv.h"

#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/CallingConv.h>
#include <llvm/Instruction.h>
#include <llvm/Instructions.h>
#include <llvm/LLVMContext.h>

#include <stdio.h>

using namespace glaz;

llvm::Module *
Component::getLlvmModule() const {
    if (module)
        return module;
    
    llvm_context = &llvm::getGlobalContext();
    module = new llvm::Module("GlazBASIC_Module", *llvm_context);
    
    // For OS X / Intel x86_64
    module->setDataLayout(
        "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64"
        ":64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0"
        ":64-s0:64:64-f80:128:128-n8:16:32:64");
    module->setTargetTriple("x86_64-apple-darwin10.0.0");
    
    // Generate all required functions
    implicit_main->getLlvmAddr(*llvm_context, *module, 0);
    
    var_map::const_iterator vend = vars.end();
    for (var_map::const_iterator it = vars.begin(); it != vend; ++it) {
        it->second->getLlvmAddr(*llvm_context, *module, 0);
    }
        
    return module;
}

const llvm::Type *
IntrinsicType::getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const {
            
    if (cached)
        return cached;
    
    switch (which) {
    case VOID:
        cached = llvm::Type::getVoidTy(context);
        break;
        
    case BOOL:
        cached = llvm::Type::getInt1Ty(context);
        break;
        
    case CHAR:
    case SCHAR:
        cached = llvm::Type::getInt8Ty(context);
        break;
        
    case WORD:
    case SWORD:
        cached = llvm::Type::getInt16Ty(context);
        break;
        
    case INT:
    case UINT:
        cached = llvm::Type::getInt32Ty(context);
        break;
        
    case INT64:
    case UINT64:
        cached = llvm::Type::getInt64Ty(context);
        break;
        
    case FLOAT:
        cached = llvm::Type::getFloatTy(context);
        break;
    
    case DOUBLE:
        cached = llvm::Type::getDoubleTy(context);
        break;
        
    default:
        assert(0 && "invalid intrinsic type");
    }
    
    return cached;
}

const llvm::Type *
Struct::getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const {
            
    if (cached)
        return cached;
    
    std::vector<const llvm::Type *> array;
    int nmembers = vars.size();
    array.reserve(nmembers);
    for (int i = 0; i < nmembers; ++i) {
        array.push_back(vars[i].second->getType()->getLlvmType(context, mod));
    }
    
    cached = llvm::StructType::get(context, array, alignment != 0);
    mod.addTypeName("struct." + name, cached);
    return cached;
}

const llvm::Type *
PointerType::getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const {
            
    if (!cached) {
        if (referenced->isVoid())
            cached = llvm::Type::getInt8Ty(context)->getPointerTo(0);
        else
            cached = referenced->getLlvmType(context, mod)->getPointerTo(0);
    }
    
    return cached;
}

const llvm::Type *
ArrayType::getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const {
    
    if (!cached)
        cached = llvm::ArrayType::get(referenced->getLlvmType(context, mod),
            sd_bounds
        );
    
    return cached;
}

const llvm::Type *
SubType::getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const {
            
    if (cached)
        return cached;
    
    const llvm::Type *rettype = rtype->getLlvmType(context, mod);
    
    int nparams = param_types.size();
    std::vector<const llvm::Type *> array;
    array.reserve(nparams);
    for (int i = 0; i < nparams; ++i) {
        array.push_back(param_types[i]->getLlvmType(context, mod));
    }
    
    if (flags & VA_IMPLICIT_LEN)
        array.push_back(llvm::Type::getInt32Ty(context));
    
    cached = llvm::FunctionType::get(
        rettype,
        array,
        (flags & VARARGS) == VARARGS
    );
    
    return cached;
}

llvm::Value *
UnaryOp::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    assert(type->typeClass() == Type::INTRINSIC && "UnaryOp assumes that it "
        "only operates on intrinsic types");
    int iid = static_cast<const IntrinsicType *>(type)->getIntrinsicId();
    bool isfp = iid == IntrinsicType::FLOAT || iid == IntrinsicType::DOUBLE;
    
    switch (op) {
    case NOT:
        return llvm::BinaryOperator::Create(
            llvm::Instruction::Xor,
            llvm::ConstantInt::get(child->getType()
                ->getLlvmType(context, mod), ~0),
            child->getLlvmValue(context, mod, block),
            "",
            block
        );
    
    case NEG:
        if (isfp)
            return llvm::BinaryOperator::Create(
                llvm::Instruction::FSub,
                llvm::ConstantFP::getZeroValueForNegation(
                    child->getType()->getLlvmType(context, mod)),
                child->getLlvmValue(context, mod, block),
                "",
                block
            );
            
        return llvm::BinaryOperator::Create(
            llvm::Instruction::Sub,
            llvm::ConstantInt::get(child->getType()
                ->getLlvmType(context, mod), 0),
            child->getLlvmValue(context, mod, block),
            "",
            block
        );
        
    // Note: llvm::ConstantFP::getZeroValueForNegation
    }
    
    assert(0 && "invalid unary operator");
    return 0;
}

llvm::Value *
BinaryOp::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    llvm::Instruction::BinaryOps llvmop;
    assert(type->typeClass() == Type::INTRINSIC && "BinaryOp assumes that it "
        "only operates on intrinsic types");
    int iid = static_cast<const IntrinsicType *>(type)->getIntrinsicId();
    bool isfp = iid == IntrinsicType::FLOAT || iid == IntrinsicType::DOUBLE;
    bool issigned = false;
    switch (iid) {
    case IntrinsicType::SCHAR:
    case IntrinsicType::SWORD:
    case IntrinsicType::INT:
    case IntrinsicType::INT64:
        issigned = true;
    }
    
    switch (op) {
    case ADD:
        if (isfp)
            llvmop = llvm::Instruction::FAdd;
        else
            llvmop = llvm::Instruction::Add;
        break;
        
    case SUB:
        if (isfp)
            llvmop = llvm::Instruction::FSub;
        else
            llvmop = llvm::Instruction::Sub;
        break;
    
    case MUL:
        if (isfp)
            llvmop = llvm::Instruction::FMul;
        else
            llvmop = llvm::Instruction::Mul;
        break;
        
    case DIV:
        if (isfp)
            llvmop = llvm::Instruction::FDiv;
        else if (issigned)
            llvmop = llvm::Instruction::SDiv;
        else
            llvmop = llvm::Instruction::UDiv;
        break;
        
    case MOD:
        if (isfp)
            llvmop = llvm::Instruction::FRem;
        else if (issigned)
            llvmop = llvm::Instruction::SRem;
        else
            llvmop = llvm::Instruction::URem;
        break;
        
    case POW:
        assert(0 && "not implemented yet");
        
    case AND:
        assert(!isfp && "operation not valid on fp numbers");
        llvmop = llvm::Instruction::And;
        break;
        
    case OR:
        assert(!isfp && "operation not valid on fp numbers");
        llvmop = llvm::Instruction::Or;
        break;
        
    case XOR:
        assert(!isfp && "operation not valid on fp numbers");
        llvmop = llvm::Instruction::Xor;
        break;
        
    case SHR:
        assert(!isfp && "operation not valid on fp numbers");
        if (issigned)
            llvmop = llvm::Instruction::AShr;
        else
            llvmop = llvm::Instruction::LShr;
        break;
        
    case SHL:
        assert(!isfp && "operation not valid on fp numbers");
        llvmop = llvm::Instruction::Shl;
        break;
        
    default:
        assert(0 && "invalid binary operator");
    }
    
    return llvm::BinaryOperator::Create(
        llvmop,
        left->getLlvmValue(context, mod, block),
        right->getLlvmValue(context, mod, block),
        "",
        block
    );
}

llvm::Value *
BinaryCmpOp::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {

    union {
        llvm::ICmpInst::Predicate llvmop;
        llvm::FCmpInst::Predicate fpop;
    };
    
    assert(left->getType()->typeClass() == Type::INTRINSIC &&
        right->getType()->typeClass() == Type::INTRINSIC &&
        "BinaryCmpOp assumes that it only operates on intrinsic types");
    int iid = static_cast<const IntrinsicType *>(left->getType())
        ->getIntrinsicId();
    bool isfp = iid == IntrinsicType::FLOAT || iid == IntrinsicType::DOUBLE;
    bool issigned = false;
    switch (iid) {
    case IntrinsicType::SCHAR:
    case IntrinsicType::SWORD:
    case IntrinsicType::INT:
    case IntrinsicType::INT64:
        issigned = true;
    }
    
    switch (op) {
    case LT:
        if (isfp)
            fpop = llvm::FCmpInst::FCMP_OLT;
        else if (issigned)
            llvmop = llvm::ICmpInst::ICMP_SLT;
        else
            llvmop = llvm::ICmpInst::ICMP_ULT;
        break;
        
    case GT:
        if (isfp)
            fpop = llvm::FCmpInst::FCMP_OGT;
        else if (issigned)
            llvmop = llvm::ICmpInst::ICMP_SGT;
        else
            llvmop = llvm::ICmpInst::ICMP_UGT;
        break;
        
    case EQ:
        if (isfp)
            fpop = llvm::FCmpInst::FCMP_OEQ;
        else
            llvmop = llvm::ICmpInst::ICMP_EQ;
        break;
        
    case LE:
        if (isfp)
            fpop = llvm::FCmpInst::FCMP_OLE;
        else if (issigned)
            llvmop = llvm::ICmpInst::ICMP_SLE;
        else
            llvmop = llvm::ICmpInst::ICMP_ULE;
        break;
        
    case GE:
        if (isfp)
            fpop = llvm::FCmpInst::FCMP_OGE;
        else if (issigned)
            llvmop = llvm::ICmpInst::ICMP_SGE;
        else
            llvmop = llvm::ICmpInst::ICMP_UGE;
        break;
        
    case NE:
        if (isfp)
            fpop = llvm::FCmpInst::FCMP_ONE;
        else
            llvmop = llvm::ICmpInst::ICMP_NE;
        break;
        
    default:
        assert(0 && "invalid comparison operator");
    }
    
    if (isfp)
        return new llvm::FCmpInst(*block, fpop,
            left->getLlvmValue(context, mod, block),
            right->getLlvmValue(context, mod, block),
            ""
        );
    
    return new llvm::ICmpInst(*block, llvmop,
        left->getLlvmValue(context, mod, block),
        right->getLlvmValue(context, mod, block),
        ""
    );
}

llvm::Value *
Deref::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    assert(0 && "not implemented!");
    return 0;
}

llvm::Value *
Deref::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    assert(0 && "not implemented!");
    return 0;
}

llvm::Value *
AddrOf::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    return expr->getLlvmAddr(context, mod, block);
}

llvm::Value *
AddrOf::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    assert(0 && "can't take address of an address");
}

llvm::Value *
PointerCast::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
            
    assert(type->typeClass() == Type::POINTER &&
        "PointerCast can only cast to pointer (obviously)");
        
    int exprtype = expr->getType()->typeClass();
        
    assert((exprtype == Type::ARRAY || exprtype == Type::POINTER) &&
        "PointerCast can only cast from array or pointer "
        "(though I need to add ints to that too)");
    
    const Type *refty = static_cast<const PointerType *>(type)->getRefType();
    const Type *from_refty;
    
    if (exprtype == Type::POINTER) {
        from_refty =
            static_cast<const PointerType *>(expr->getType())->getRefType();
    } else {
        from_refty =
            static_cast<const ArrayType *>(expr->getType())->getRefType();
    }
    
    if (refty != from_refty) {
        
        // These are initialized here but only used if it is determined
        // the types actually are intrinsic.
        const IntrinsicType *it, *fromit;
        it = static_cast<const IntrinsicType *>(refty);
        fromit = static_cast<const IntrinsicType *>(from_refty);
        
        // void* is treated as i8* in llvm. Therefore, if we're casting between
        // void* and char*, then we don't need to cast.

        if (!(((refty->isVoid() || from_refty->isVoid())
            && (refty->typeClass() == Type::INTRINSIC
                && (it->getIntrinsicId() == IntrinsicType::CHAR
                    || it->getIntrinsicId() == IntrinsicType::SCHAR)
                ))
            || (from_refty->typeClass() == Type::INTRINSIC
                && (fromit->getIntrinsicId() == IntrinsicType::CHAR
                    || fromit->getIntrinsicId() == IntrinsicType::SCHAR)
                )
            )) {
            
            return new llvm::BitCastInst(
                expr->getLlvmValue(context, mod, block),
                type->getLlvmType(context, mod),
                "", block
            );
        }
    }
    return expr->getLlvmValue(context, mod, block);
}

llvm::Value *
SignCast::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    return expr->getLlvmValue(context, mod, block);
}

// The signed-ness of the extend operator depends on the signed-ness of the
// type it is coming from, not the one it is going to.
llvm::Value *
IntWideningCast::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    bool issigned = false;
    
    // Preconditions:
    assert(expr->getType()->typeClass() == Type::INTRINSIC &&
        "Int*Cast only works on intrinsic (integer) types!");
        
    assert(type->typeClass() == Type::INTRINSIC &&
        "Int*Cast only casts to intrinsic (integer) types!");
    
    const IntrinsicType *itfrom =
        static_cast<const IntrinsicType *>(expr->getType());
    //const IntrinsicType *itto = static_cast<const IntrinsicType *>(type);
    
    int iidfrom = itfrom->getIntrinsicId();
    //int iidto = itto->getIntrinsicId();
    
    switch (iidfrom) {
    case IntrinsicType::SCHAR:
    case IntrinsicType::SWORD:
    case IntrinsicType::INT:
        issigned = true;
        break;
    
    case IntrinsicType::FLOAT:
    case IntrinsicType::DOUBLE:
        assert(0 && "int casts not valid on fp types.");
    }
    
    if (issigned)
        return new llvm::SExtInst(
            expr->getLlvmValue(context, mod, block),
            type->getLlvmType(context, mod),
            "",
            block
        );
        
    return new llvm::ZExtInst(
        expr->getLlvmValue(context, mod, block),
        type->getLlvmType(context, mod),
        "",
        block
    );
}

llvm::Value *
IntNarrowingCast::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    // Preconditions:
    assert(expr->getType()->typeClass() == Type::INTRINSIC &&
        "Int*Cast only works on intrinsic (integer) types!");
        
    assert(type->typeClass() == Type::INTRINSIC &&
        "Int*Cast only casts to intrinsic (integer) types!");
        
    return new llvm::TruncInst(
        expr->getLlvmValue(context, mod, block),
        type->getLlvmType(context, mod),
        "",
        block
    );
}

llvm::Value *
IntToFpCast::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    assert(0 && "not implemented!");
}

llvm::Value *
FpToIntCast::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    assert(0 && "not implemented!");
}

llvm::Value *
LitConstant::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    // String constant?
    if (type->typeClass() == Type::ARRAY) {
        // To access a string constant, we use the constant expression version
        // of the getelementptr instruction.
        getLlvmAddr(context, mod, block);
        
        llvm::Constant *zero =
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        
        llvm::Value *zero_arr[2] = { zero, zero };
        
        return llvm::ConstantExpr::getInBoundsGetElementPtr(
            cached_gv, zero_arr, 2
        );
    }
    
    // Otherwise it's an integer constant.
    int iid = static_cast<const IntrinsicType *>(type)->getIntrinsicId();
        
    switch (iid) {
    case IntrinsicType::BOOL:
        if (value.b)
            return llvm::ConstantInt::getTrue(context);
        else
            return llvm::ConstantInt::getFalse(context);
            
    case IntrinsicType::INT:
        return llvm::ConstantInt::get(
            type->getLlvmType(context, mod),
            value.i,
            true
        );
        
    case IntrinsicType::UINT:
        return llvm::ConstantInt::get(
            type->getLlvmType(context, mod),
            value.u,
            false
        );
        
    case IntrinsicType::INT64:
        return llvm::ConstantInt::get(
            type->getLlvmType(context, mod),
            value.ll,
            true
        );
        
    case IntrinsicType::UINT64:
        return llvm::ConstantInt::get(
            type->getLlvmType(context, mod),
            value.ull,
            false
        );
        
    case IntrinsicType::FLOAT:
        return llvm::ConstantFP::get(
            type->getLlvmType(context, mod),
            value.f
        );
        
    case IntrinsicType::DOUBLE:
        return llvm::ConstantFP::get(
            type->getLlvmType(context, mod),
            value.d
        );
    }
    
    assert(0 && "invalid lit constant type");
    return 0;
}

llvm::Value *
LitConstant::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    if (type->typeClass() != Type::ARRAY) {
        assert(0 && "can't take address of integer constant");
    }
    
    if (!cached_gv) {
        llvm::Constant *strcon = llvm::ConstantArray::get(
            context,
            str,
            true
        );
                
        cached_gv = new llvm::GlobalVariable(mod,
                strcon->getType(),
                true, llvm::GlobalValue::InternalLinkage,
                strcon, "", 0, false);
        cached_gv->setUnnamedAddr(true);
    }
    
    return cached_gv;
}

llvm::Value *
Var::getLlvmValue(llvm::LLVMContext &context,
    llvm::Module &mod, llvm::BasicBlock *block) const {
    
    llvm::LoadInst *load = new llvm::LoadInst(getLlvmAddr(context, mod, block));
    
    assert(block && "load instruction must be inside a function");
    block->getInstList().push_back(load);
    return load;
}

llvm::Value *
GlobalVar::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    if (cached_gv)
        return cached_gv;
    
    // No initializer present? Numbers -> 0, strings/structs->null.
    llvm::Constant *init;
    if (!initializer) {
        if (type->typeClass() == Type::INTRINSIC) {
            init = llvm::ConstantInt::get(type->getLlvmType(context, mod), 0);
        } else if (type->typeClass() == Type::STRUCT) {
            init = llvm::ConstantAggregateZero::get(
                type->getLlvmType(context, mod)
            );
        } else if (type->typeClass() == Type::ARRAY) {
            init = llvm::ConstantAggregateZero::get(
                type->getLlvmType(context, mod)
            );
        } else {
            init = llvm::ConstantPointerNull::get(
                static_cast<const llvm::PointerType *>(
                    type->getLlvmType(context, mod)
                )
            );
        }
    } else {
        init = static_cast<llvm::Constant *>(
            initializer->getLlvmValue(context, mod, 0)
        );
    }
        
    cached_gv = new llvm::GlobalVariable(mod, type->getLlvmType(context, mod),
        false, llvm::GlobalValue::ExternalLinkage, init, name);
    return cached_gv;
}

llvm::Value *
LocalVar::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    assert(cached_alloca && "use createLocal before calling getLlvmAddr");
    
    return cached_alloca;
}

void
LocalVar::createLocal(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) {
            
    cached_alloca = new llvm::AllocaInst(type->getLlvmType(context, mod),
        name, block);
}

void
LocalVar::genInitializer(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block) {
    
    llvm::Value *init;
    if (!initializer) {
        // In IBasic, locals default to 0/null/empty string/whatever.
        if (type->typeClass() == Type::INTRINSIC) {
            init = llvm::ConstantInt::get(type->getLlvmType(context, mod), 0);
        } else if (type->typeClass() == Type::STRUCT) {
            init = llvm::ConstantAggregateZero::get(
                type->getLlvmType(context, mod)
            );
        } else if (type->typeClass() == Type::ARRAY) {
            init = llvm::ConstantAggregateZero::get(
                type->getLlvmType(context, mod)
            );
        } else {
            init = llvm::ConstantPointerNull::get(
                static_cast<const llvm::PointerType *>(
                    type->getLlvmType(context, mod)
                )
            );
        }
    } else {
        init = initializer->getLlvmValue(context, mod, block);
    }
    
    // The false, since I keep forgetting, means it isn't volatile.
    new llvm::StoreInst(init, cached_alloca, false, block);
}

llvm::Value *
Param::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    assert(cached_alloca && "use bindParam before calling getLlvmAddr");
    
    return cached_alloca;
}

void
Param::bindParam(llvm::Value *val, llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) {
    
    cached_alloca = new llvm::AllocaInst(type->getLlvmType(context, mod),
        name + ".addr", block);
    
    cached_val = val;
    val->setName(name);
}

void
Param::genInitializer(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block) {
    
    new llvm::StoreInst(cached_val, cached_alloca, false, block);
}

ArrayIndexer::~ArrayIndexer() {

}

llvm::Value *
ArrayIndexer::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    llvm::LoadInst *load = new llvm::LoadInst(getLlvmAddr(context, mod, block));
    block->getInstList().push_back(load);
    return load;
}

llvm::Value *
ArrayIndexer::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    
    if (var->getType()->typeClass() == Type::POINTER) {
        /*
         * If for example, we have an int* variable, it is of type i32*,
         * and the name of it is actually type i32**. Therefore, we have
         * to load it first to get an i32*, and then gep it to get an i32.
         */
        
        llvm::LoadInst *loaded =
            new llvm::LoadInst(var->getLlvmAddr(context, mod, block));
        block->getInstList().push_back(loaded);
        llvm::Value *offset = bounds[0]->getLlvmValue(context, mod, block);
        llvm::GetElementPtrInst *gep =
            llvm::GetElementPtrInst::Create(loaded, offset, "", block);
        return gep;
    }
    
    llvm::Value *offset = 0;
    const ArrayType *ty = static_cast<const ArrayType *>(var->getType());
    unsigned nbounds = static_cast<const ArrayType *>(var->getType())
        ->getNumBounds();
    for (unsigned i = 0; i < nbounds; ++i) {
        if (offset) {
            // Multiply it by the last max bound and add the new bound.
            llvm::Value *maxb = llvm::ConstantInt::get(
                offset->getType(),
                ty->getBound(i),
                false
            );
            
            offset = llvm::BinaryOperator::Create(
                llvm::Instruction::Mul,
                offset,
                maxb,
                "",
                block
            );
            
            offset = llvm::BinaryOperator::Create(
                llvm::Instruction::Add,
                offset,
                bounds[i]->getLlvmValue(context, mod, block),
                "",
                block
            );
        } else {
            offset = bounds[i]->getLlvmValue(context, mod, block);
        }
    }
    
    std::vector<llvm::Value *> vals;
    vals.push_back(llvm::ConstantInt::get(offset->getType(),
        0, false));
    vals.push_back(offset);
    llvm::GetElementPtrInst *gep =
        llvm::GetElementPtrInst::Create(
            var->getLlvmAddr(context, mod, block),
            vals.begin(),
            vals.end(),
            "",
            block
        );
    return gep;
}

llvm::Value *
StructAccessor::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    assert(0 && "not implemented!");
}

llvm::Value *
StructAccessor::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    assert(0 && "not implemented!");
}

// Loads all the arguments for a call and returns a vector containing them.
std::vector<llvm::Value *>
callAssist(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block, const SubType *subty,
        std::vector<Expression *> exprlist) {
                    
    assert(!(((subty->getFlags() & SubType::VA_IMPLICIT_LEN) != 0) &&
        ((subty->getFlags() & SubType::VA_IMPLICIT_NULL) != 0)) &&
        "can have either implicit len or null, but not both");
    assert(block && "can't create call outside block");
    
    std::vector<llvm::Value *> array;
    int nparams = exprlist.size();
    if (subty->getFlags() &
            (SubType::VA_IMPLICIT_LEN | SubType::VA_IMPLICIT_NULL))
        array.reserve(nparams+1);
    else
        array.reserve(nparams);
    
    // If we're supposed to push an additional len value ("x(len, ...)"),
    // it goes after this parameter (-1 for no len pushed).
    int atlen = -1;
        
    if (subty->getFlags() & (SubType::VA_IMPLICIT_LEN))
        atlen = subty->paramSize() - 1;
    
    for (int i = 0; i < nparams; ++i) {
        array.push_back(exprlist[i]->getLlvmValue(context, mod, block));
        
        if (i == atlen) {
            array.push_back(llvm::ConstantInt::get(
                llvm::Type::getInt32Ty(context),
                nparams - atlen - 1,
                false)
            );
        }
    }
    
    if ((subty->getFlags() & SubType::VA_IMPLICIT_NULL) != 0
        || ((subty->getFlags() & SubType::VA_IMPLICIT_LEN) != 0
            && nparams == 0)) {
                
        array.push_back(llvm::ConstantInt::get(
            llvm::Type::getInt32Ty(context),
            0,
            false)
        );
    }
        
    return array;
}

llvm::Value *
CallExpr::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    std::vector<llvm::Value *> args =
        callAssist(context, mod, block, subty, exprlist);
        
    return llvm::CallInst::Create(sub->getLlvmAddr(context, mod, block),
        args.begin(), args.end(), "", block);
}

llvm::Value *
PtrCallExpr::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {

    std::vector<llvm::Value *> args =
        callAssist(context, mod, block, subty, exprlist);
        
    return llvm::CallInst::Create(var->getLlvmValue(context, mod, block),
        args.begin(), args.end(), "", block);
}

bool Label::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
    assert(0 && "not implemented");
    return false;
}

const llvm::BasicBlock *
Label::getLlvmBasicBlock() const {
    assert(0 && "not implemented");
    return 0;
}

bool CallStmt::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
    
    return ce->getLlvmValue(context, mod, block) != 0;
}

bool Goto::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
    assert(0 && "not implemented");
    return false;
}

bool Assign::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {

    llvm::Value *addr = left->getLlvmAddr(context, mod, block);
    llvm::Value *rightval = right->getLlvmValue(context, mod, block);
        
    new llvm::StoreInst(
        rightval,
        addr,
        false,
        block
    );
    
    return true;
}

bool Return::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
            
    if (expr) {
        llvm::Value *value = expr->getLlvmValue(context, mod, block);
        llvm::ReturnInst::Create(context, value, block);
    } else {
        llvm::ReturnInst::Create(context, block);
    }
    
    return true;
}

bool IfBlock::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
    
    llvm::Value *compare = expr->getLlvmValue(context, mod, block);
    llvm::BasicBlock *trueblock, *falseblock = 0, *continueblock;
    
    trueblock = llvm::BasicBlock::Create(context, ".iftrue",
        block->getParent());
        
    if (else_first)
        falseblock = llvm::BasicBlock::Create(context, ".iffalse",
            block->getParent());
            
    continueblock = llvm::BasicBlock::Create(context, ".endif",
        block->getParent());
        
    llvm::BranchInst::Create(trueblock,
        falseblock ? falseblock : continueblock, compare, block);
    
    InOrderNode *node = first;
    while (node) {
        node->genLlvm(context, mod, trueblock);
        node = node->next;
    }
    if (!trueblock->getTerminator())
        llvm::BranchInst::Create(continueblock, trueblock);
    
    node = else_first;
    while (node) {
        node->genLlvm(context, mod, falseblock);
        node = node->next;
    }
    if (else_first && !falseblock->getTerminator())
        llvm::BranchInst::Create(continueblock, falseblock);

    block = continueblock;
    
    return true;
}

bool SelectBlock::genLlvmSwitch(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
    
    llvm::Value *compare = expr->getLlvmValue(context, mod, block);
    
    // No cases? Don't emit any code. But _do_ emit the expression (above);
    // there might be side effects.
    if (nr_of_cases == 0)
        return true;
        
    llvm::BasicBlock *first_block = block;
    llvm::BasicBlock *default_block = 0;
    llvm::BasicBlock *current_block;
    llvm::BasicBlock *continue_block = llvm::BasicBlock::Create(
        context, ".endselect"
    );
    
    llvm::BasicBlock **blocks = new llvm::BasicBlock *[nr_of_cases];
    llvm::ConstantInt **exprs = new llvm::ConstantInt *[nr_of_cases];
    
    CaseBlock *cb = first_case;
    int bb_idx = 0, expr_idx = 0;
    while (cb) {
        if (cb->expr) {
            current_block = blocks[bb_idx++] = llvm::BasicBlock::Create(
                context, ".case", block->getParent());
            exprs[expr_idx++] = static_cast<llvm::ConstantInt *>(
                cb->expr->getLlvmValue(context, mod, 0)
            );
        } else {
            current_block = blocks[bb_idx++] = llvm::BasicBlock::Create(
                context, ".default", block->getParent());
            default_block = current_block;
        }
        
        InOrderNode *case_stmt = cb->first;
        while (case_stmt) {
            case_stmt->genLlvm(context, mod, current_block);
            case_stmt = case_stmt->next;
        }
        
        // Create an implicit 'break'
        if (!current_block->getTerminator())
            llvm::BranchInst::Create(continue_block, current_block);
        
        cb = cb->next;
    }
    
    // Insert the continue block and create the switch instruction.
    block->getParent()->getBasicBlockList().push_back(continue_block);
    //continue_block->moveAfter(current_block);
    
    int cases_incl_default = nr_of_cases;
    if (!default_block) {
        default_block = continue_block;
        ++cases_incl_default;
    } else if (nr_of_cases == 1) {
        // The only thing present was a default. In that case, just emit
        // a jump to the default instead of a switch instruction. LLVM's
        // optimizers will get rid of this jump.
        llvm::BranchInst::Create(default_block, first_block);
        
        delete[] blocks;
        delete[] exprs;
        block = continue_block;
        return true;
    }
    
    llvm::SwitchInst *sw = llvm::SwitchInst::Create(compare, default_block,
        cases_incl_default, first_block);
        
    for (int i = 0, j = 0; i < nr_of_cases; ++i) {
        if (blocks[i] == default_block)
            continue;
        
        sw->addCase(exprs[j++], blocks[i]);
    }
    
    delete[] blocks;
    delete[] exprs;
    
    block = continue_block;
    return true;
}

bool SelectBlock::genLlvmIf(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
            
    assert(0 && "not implemented yet");
}

bool SelectBlock::genLlvm(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const {
    
    if (has_nonconst_case)
        return genLlvmIf(context, mod, block);
    else
        return genLlvmSwitch(context, mod, block);
}

bool ForBlock::genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const {
    
    assert(0 && "this one is hard...");
}

bool WhileBlock::genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const {
    
    llvm::BasicBlock *first_block, *cont_block, *body, *break_block;
    first_block = block;
    cont_block = llvm::BasicBlock::Create(context, ".continue",
        block->getParent());
    body = llvm::BasicBlock::Create(context, ".while");
    break_block = llvm::BasicBlock::Create(context, ".break");
        
    llvm::BranchInst::Create(cont_block, first_block);
    
    // Create an if (cond) goto body; else goto done;
    // TODO: coerce expr to boolean, get its inverse.
    llvm::Value *compare = expr->getLlvmValue(context, mod, cont_block);
    llvm::BranchInst::Create(body, break_block, compare, cont_block);
    
    cont_block->getParent()->getBasicBlockList().push_back(body);
    
    InOrderNode *node = first;
    while (node) {
        node->genLlvm(context, mod, body);
        node = node->next;
    }
    
    if (!body->getTerminator())
        llvm::BranchInst::Create(cont_block, body);
    
    body->getParent()->getBasicBlockList().push_back(break_block);
    block = break_block;
    
    return true;
}

bool DoBlock::genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const {
    
    llvm::BasicBlock *first_block, *body, *orig_body, *break_block;
    first_block = block;
    orig_body = body = llvm::BasicBlock::Create(context, ".do",
        block->getParent());
    break_block = llvm::BasicBlock::Create(context, ".break");
    
    llvm::BranchInst::Create(body, first_block);
    
    InOrderNode *node = first;
    while (node) {
        node->genLlvm(context, mod, body);
        node = node->next;
    }
    
    if (!body->getTerminator()) {
        // Create an if (cond) goto done; else goto body; (b/c the until)
        llvm::Value *compare = expr->getLlvmValue(context, mod, body);
        llvm::BranchInst::Create(break_block, orig_body, compare, body);
    }
    
    body->getParent()->getBasicBlockList().push_back(break_block);
    block = break_block;
    
    return true;
}

llvm::Value *
Sub::getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
    
    // I know this is frowned upon, but it keeps things easy, since
    // getLlvmFunction does a lot of things.
    return const_cast<Sub *>(this)->getLlvmFunction(context, &mod);
}

llvm::Value *
Sub::getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const {
        
    return const_cast<Sub *>(this)->getLlvmFunction(context, &mod);
}

llvm::Constant *
Sub::getLlvmFunction(llvm::LLVMContext &context, llvm::Module *mod) {
    if (function)
        return function;
    
    const llvm::FunctionType *fnty =
        static_cast<const llvm::FunctionType *>(
            this->type->getLlvmType(context, *mod)
        );
        
    std::string gen_name;
    if (flags & HAS_ALIAS)
        gen_name = aliasname;
    else
        gen_name = name;
    
    //function = static_cast<llvm::Function *>(
    //  mod->getOrInsertFunction(gen_name, fnty)
    //);
    llvm::Function *fn = mod->getFunction(gen_name);
    function = fn;
    
    llvm::GlobalValue::LinkageTypes linkage =
        llvm::GlobalValue::InternalLinkage;
    
    if (gen_name == "_GB_main")
        linkage = llvm::GlobalValue::ExternalLinkage;
    
    if (!fn) {
        fn = llvm::Function::Create(
            fnty,
            linkage,
            gen_name,
            mod
        );
        function = fn;
    }
    
    //if ((this->type->getFlags() & SubType::CDECL) == SubType::CDECL)
        fn->setCallingConv(llvm::CallingConv::C);
    
    // If it's just a declare, don't generate any code for it.
    if ((flags & IMPLEMENTED) == 0)
        return function;
    
    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(
            context,
            ".entry",
            fn
        );
        
    // Generate params and locals
    // Because of the way subs are generated, we know that all the params
    // are at the beginning of the list, and all the locals at the end.
    var_list::iterator it = varlist.begin(), varend = varlist.end();
    llvm::Function::arg_iterator args = fn->arg_begin();
    for (; it != varend; ++it) {
        if ((*it)->exprClass() != Expression::PARAM)
            break;
        
        static_cast<Param *>(*it)->bindParam(args++, context, *mod, entry);
    }
    
    for (; it != varend; ++it) {
        static_cast<LocalVar *>(*it)->createLocal(context, *mod, entry);
    }
    
    // Now initialize all the locals, and alloca's generated by params.
    it = varlist.begin();
    for (; it != varend; ++it) {
        (*it)->genInitializer(context, *mod, entry);
    }
        
    for (InOrderNode *node = first; node != 0; node = node->next) {
        node->genLlvm(context, *mod, entry);
    }
    
    // If there was no explicit END, insert an implicit one.
    if (!entry->getTerminator())
        llvm::ReturnInst::Create(context, entry);
    
    return function;
}

