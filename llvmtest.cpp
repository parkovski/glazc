#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/PassManager.h>
#include <llvm/CallingConv.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Support/IRBuilder.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/LLVMContext.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/Target/TargetSelect.h>

#include <iostream>

using namespace llvm;

Function *getMulAddFunction(Module *);
Function *getGCDFunction(Module *);

void llvmtest() {
    Module *mod = new Module("test", getGlobalContext());
    Function *mul_add = getMulAddFunction(mod), *gcd = getGCDFunction(mod);
    
    verifyModule(*mod, PrintMessageAction);
    
    PassManager pm;
    pm.add(createPrintModulePass(&outs()));
    pm.run(*mod);
    
    InitializeNativeTarget();
    ExecutionEngine *engine = EngineBuilder(mod).create();
    void *fptr = engine->getPointerToFunction(mul_add);
    int(*native)(int,int,int) = (int(*)(int,int,int))(intptr_t)fptr;
    fptr = engine->getPointerToFunction(gcd);
    int(*native2)(int,int) = (int(*)(int,int))(intptr_t)fptr;
    std::cout << "mul_add(2,3,4) = " << native(2,3,4) << std::endl;
    std::cout << "gcd(35,100) = " << native2(35,100) << std::endl;
    
    delete engine;
}

Function *getMulAddFunction(Module *mod) {
    LLVMContext &context = getGlobalContext();
    Constant *c = mod->getOrInsertFunction("mul_add",
        IntegerType::get(context, 32), // return type
        IntegerType::get(context, 32), // arg types
        IntegerType::get(context, 32),
        IntegerType::get(context, 32),
        NULL
        );
    
    Function *mul_add = cast<Function>(c);
    mul_add->setCallingConv(CallingConv::C);
    
    Function::arg_iterator args = mul_add->arg_begin();
    Value *x = args++;
    x->setName("x");
    Value *y = args++;
    y->setName("y");
    Value *z = args++;
    z->setName("z");
    
    BasicBlock *block = BasicBlock::Create(context, "entry",
        mul_add);
    IRBuilder<> builder(block);
    
    Value *temp = builder.CreateBinOp(Instruction::Mul, x, y, "tmp");
    Value *temp2 = builder.CreateBinOp(Instruction::Add, temp, z, "tmp");
    
    builder.CreateRet(temp2);
    
    return mul_add;
}

/*
unsigned gcd(unsigned x, unsigned y) {
  if(x == y) {
    return x;
  } else if(x < y) {
    return gcd(x, y - x);
  } else {
    return gcd(x - y, y);
  }
}
*/

Function *getGCDFunction(Module *mod) {
    LLVMContext &context = getGlobalContext();
    Constant *c = mod->getOrInsertFunction("gcd",
        IntegerType::get(context, 32),
        IntegerType::get(context, 32),
        IntegerType::get(context, 32),
        NULL
        );
    
    Function *gcd = cast<Function>(c);
    gcd->setCallingConv(CallingConv::C);
    
    Function::arg_iterator args = gcd->arg_begin();
    Value *x = args++;
    x->setName("x");
    Value *y = args++;
    y->setName("y");
    
    BasicBlock *entry = BasicBlock::Create(context, "entry", gcd);
    BasicBlock *ifeq = BasicBlock::Create(context, "ifeq", gcd);
    BasicBlock *else1 = BasicBlock::Create(context, "else", gcd);
    BasicBlock *ifless = BasicBlock::Create(context, "ifless", gcd);
    BasicBlock *else2 = BasicBlock::Create(context, "else", gcd);
    IRBuilder<> builder(entry);
    
    Value *areEqual = builder.CreateICmpEQ(x, y, "tmp");
    builder.CreateCondBr(areEqual, ifeq, else1);
    
    builder.SetInsertPoint(ifeq);
    builder.CreateRet(x);
    
    builder.SetInsertPoint(else1);
    Value *xLessY = builder.CreateICmpULT(x, y, "tmp");
    builder.CreateCondBr(xLessY, ifless, else2);
    
    builder.SetInsertPoint(ifless);
    Value *yMinusX = builder.CreateSub(y, x, "tmp");
    std::vector<Value*> args1;
    args1.push_back(x);
    args1.push_back(yMinusX);
    Value *recur_1 = builder.CreateCall(gcd, args1.begin(), args1.end(),
        "tmp");
    builder.CreateRet(recur_1);
    
    builder.SetInsertPoint(else2);
    Value *xMinusY = builder.CreateSub(x, y, "tmp");
    std::vector<Value*> args2;
    args2.push_back(xMinusY);
    args2.push_back(y);
    Value *recur_2 = builder.CreateCall(gcd, args2.begin(), args2.end(),
        "tmp");
    builder.CreateRet(recur_2);
    
    return gcd;
}

