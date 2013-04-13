#ifndef AST_H
#define AST_H

#include "ast-expr.h"

#include <cassert>
#include <unordered_map>
#include <string>
#include <vector>

namespace llvm {
    class LLVMContext;
    class Module;
    class Constant;
    class Function;
    class GlobalVariable;
    class AllocaInst;
    class Type;
    class StructType;
    class FunctionType;
    class Value;
    class BasicBlock;
    class LoadInst;
}

namespace glaz {

struct Token;

class Sub;
class Label;
class Var;
class Type;
class SubType;
class Expression;

class Component {
public:
    typedef std::unordered_map<std::string, Var *> var_map;
    
    // This uses shared_ptrs because not all types will be stored here.
    typedef std::unordered_map<std::string, const Type *> type_map;
        
private:
    // This includes all vars, subs, consts, setids, etc.
    var_map vars;
    type_map types;
    // For convenience, to avoid lookups.
    const Type *void_type;
    
    Sub *implicit_main;
    
    mutable llvm::LLVMContext *llvm_context;
    mutable llvm::Module *module;

    // Helpers for constructing from a tree
    bool pass1(Token *tree);
    bool pass1_in_control_stmt(Token *&stmt, Sub *sub, bool &foundRet);
    bool pass1_if(Token *&tree, Sub *sub, bool &foundRet);
    bool pass1_select(Token *&tree, Sub *sub, bool &foundRet);
    bool pass1_for(Token *tree, Sub *sub, bool &foundRet);
    bool pass1_do_or_while(Token *tree, Sub *sub, bool &foundRet);
    bool pass1_sub(Token *&tree);
    bool pass1_declare(Token *tree);
    bool pass1_label(Token *tree, Sub *sub);
    bool pass1_type(Token *tree);
    bool addSubOrDeclare(Token *tree, Sub *&me);
    bool paramListsEqual(Token *&tree, Sub *sub) const;
    
    bool pass2(Token *tree, Sub *sub);
    bool pass2(Token *tree) { return pass2(tree, implicit_main); }
    bool pass2_after_end(Token *tree);
    bool pass2_type(Token *tree);
    bool pass2_def(Token *tree, Sub *sub);
    bool pass2_const_or_setid(Token *tree, Sub *sub);
    bool pass2_label(Token *tree, Sub *sub);
    bool pass2_goto(Token *tree, Sub *sub);
    bool pass2_assign(Token *tree, Sub *sub);
    bool pass2_return(Token *tree, Sub *sub);
    bool pass2_end(Token *tree, Sub *sub);
    Expression *pass2_expr(const Token *tree, Sub *sub);
    Expression *pass2_uexpr(const Token *tree, int op, Sub *sub);
    Expression *pass2_bexpr(const Token *tree, int op, Sub *sub);
    Expression *pass2_cmpexpr(const Token *tree, int op, Sub *sub);
    Expression *pass2_litconst(const Token *tree);
    Expression *pass2_call(const Token *tree, Sub *sub);
    bool pass2_call_stmt(const Token *tree, Sub *sub);
    Expression *pass2_cast(const Token *tree, Sub *sub);
    Expression *pass2_addrof(const Token *tree, Sub *sub);
    Expression *pass2_deref(const Token *tree, Sub *sub);
    Expression *pass2_structmember(const Token *tree, Sub *sub);
    Expression *pass2_arrayindex(Var *var, const Token *tree, Sub *sub);
    bool pass2_if(Token *&tree, Sub *sub);
    bool pass2_select(Token *&tree, Sub *sub);
    bool pass2_for(Token *&tree, Sub *sub);
    bool pass2_while(Token *&tree, Sub *sub);
    bool pass2_do(Token *&tree, Sub *sub);
    
    const Type *getType(const Token *tok) const;
    Var *resolveVar(const std::string name, const Sub *sub) const;
    
public:
    explicit Component();
    ~Component();
    
    static Component *fromTree(Token *tree, bool del = true);
    //void toXml(std::ostream &file) const;
    
    Sub *getMain() { return implicit_main; }
    
    Sub *getSub(const std::string name) const;
    Sub *insertSub(const std::string name, int flags);
    
    const Type *getType(const std::string name) const;
    bool insertType(const std::string name, const Type *type);
    
    Var *getVar(const std::string name) const;
    bool insertVar(const std::string name, Var *var);
    
    llvm::Module *getLlvmModule() const;
    // Not valid to call this until getLlvmModule has been called.
    llvm::LLVMContext &getLlvmContext() const { return *llvm_context; }
};


//==============================================================================


class PointerType;

class Type {
    mutable const PointerType *ptrtome;
    
public:
    enum {
        INVALID = 0,
        INTRINSIC,
        STRUCT,
        POINTER,
        ARRAY,
        SUBTYPE,
        STRING
    };
    
    Type() : ptrtome(0) { }
    virtual ~Type() { }
    
    //virtual void toXml(std::ostream &file, int indent) const { }
    // Returns which class of type it is (I know, awkward wording). E.g.,
    // intrinsic, structure, pointer, array, etc.
    virtual int typeClass() const = 0;
    virtual bool operator==(const Type &rhs) const = 0;
    virtual const std::string getName() const = 0;
    bool operator!=(const Type &rhs) const { return !(*this == rhs); }
    bool isVoid() const; // For easy test of void or not
    const PointerType *getPtrType() const;
    
    virtual llvm::Type *getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const = 0;
};

// This might be better called NumericType.
class IntrinsicType : public Type {
public:
    enum IntrinsicId {
        VOID = 0,
        BOOL,
        CHAR, SCHAR,
        WORD, SWORD,
        UINT, INT,
        UINT64, INT64,
        FLOAT, DOUBLE
    };
    
private:
    IntrinsicId which;
    const std::string name;
    
    mutable llvm::Type *cached;
    
public:
    explicit IntrinsicType(IntrinsicId which);
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int typeClass() const { return INTRINSIC; }
    virtual bool operator==(const Type &rhs) const;
    virtual const std::string getName() const { return name; }
    IntrinsicId getIntrinsicId() const { return which; }
    
    virtual llvm::Type *getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const;
};

class Struct : public Type {
public:
    typedef std::vector<std::pair<std::string, Var *> > var_list;
    
private:
    const std::string name;
    // 0 means to use the default alignment.
    const unsigned alignment;
    var_list vars;
    bool is_implemented; // for forward references
    
    mutable llvm::StructType *cached;
    
public:
    explicit Struct(const std::string name) :
        Type(),
        name(name),
        alignment(0),
        is_implemented(false),
        cached(0) { }
        
    explicit Struct(const std::string name, unsigned align) :
        Type(),
        name(name),
        alignment(align),
        is_implemented(false),
        cached(0) { }
    
    virtual ~Struct();
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int typeClass() const { return STRUCT; }
    virtual bool operator==(const Type &rhs) const;
    virtual const std::string getName() const { return name; }
    
    void setImplFlag() { is_implemented = true; }
    bool isImplemented() const { return is_implemented; }
    bool addVar(std::string name, Var *var);
    
    virtual llvm::Type *getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const;
};

class PointerType : public Type {
    const Type *referenced;
    const std::string name;
    
    mutable llvm::Type *cached;
    
    friend const PointerType *Type::getPtrType() const;
    
    explicit PointerType(const Type *ty) :
        Type(),
        referenced(ty),
        name(ty->isVoid() ? "pointer" : ty->getName() + "*"),
        cached(0) { }
    
public:
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int typeClass() const { return POINTER; }
    virtual bool operator==(const Type &rhs) const;
    virtual const std::string getName() const { return name; }
    
    const Type *getRefType() const { return referenced; }
    
    virtual llvm::Type *getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const;
};

class ArrayType : public Type {
    const Type *referenced;
    const std::string name;
    
    mutable llvm::Type *cached;
    
    std::vector<unsigned> bounds;
    unsigned sd_bounds;
    
public:
    explicit ArrayType(const Type *ty, unsigned bounds);
    explicit ArrayType(const Type *ty, std::vector<unsigned> bounds);
            
    virtual int typeClass() const { return ARRAY; }
    virtual bool operator==(const Type &rhs) const;
    virtual const std::string getName() const { return name; }
    
    const Type *getRefType() const { return referenced; }
    
    unsigned getNumBounds() const { return bounds.size(); }
    unsigned getBound(unsigned n) const { return bounds[n]; }
    
    virtual llvm::Type *getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const;
};

class SubType : public Type {
public:
    typedef std::vector<const Type *> param_list;
    
    enum {
        CDECL = 0x1,
        VARARGS = 0x2,
        VA_IMPLICIT_LEN = 0x4,
        VA_IMPLICIT_NULL = 0x8
    };

private:
    const Type *rtype;
    param_list param_types;
    // We have to be able to generate a name after pushing params.
    mutable std::string name;
    int flags;
    
    mutable llvm::Type *cached;
    
public:
    explicit SubType(std::string name) :
        Type(),
        rtype(0),
        param_types(),
        name(name),
        flags(0),
        cached(0) { }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int typeClass() const { return SUBTYPE; }
    virtual bool operator==(const Type &rhs) const;
    
    // Note: name is finalized when you call this, so you should not add any
    // more parameters or set the return type after calling this.
    virtual const std::string getName() const;
    
    int getFlags() const { return flags; }
    void addFlags(int fl) { flags |= fl; }
    
    void setReturnType(const Type *rtype) {
        this->rtype = rtype;
    }
    const Type *getReturnType() const { return rtype; }
    
    void addParamType(const Type *ty) {
        param_types.push_back(ty);
    }
    param_list::const_iterator param_begin() const {
        return param_types.begin();
    }
    param_list::const_iterator param_end() const {
        return param_types.end();
    }
    int paramSize() const { return param_types.size(); }
    const Type *getParam(int i) const { return param_types[i]; }
    
    virtual llvm::Type *getLlvmType(llvm::LLVMContext &context,
        llvm::Module &mod) const;
};


//==============================================================================


class UnaryOp : public Expression {
    int op;
    Expression *child;
    
public:
    enum {
        INVALID = 0,
        NEG,
        NOT
    };
    
    explicit UnaryOp(int op, Expression *ex) :
        Expression(ex->getType()), op(op), child(ex) { }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int exprClass() const { return UNARYOP; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

// Tries to implicitly convert expr to type. If it succeeds, expr refers to the
// new expression. If it fails, expr is unchanged.
bool implicitConvert(Expression *&expr, const Type *type, Component *c);
// Tries to make left and right the same type. If it succeeds, left and right
// refer to expressions that are the same type. If it fails, they are unchanged.
bool implicitConvert(Expression *&left, Expression *&right, Component *c);

class BinaryOp : public Expression {
protected:
    int op;
    Expression *left;
    Expression *right;
    
public:
    enum {
        INVALID = 0,
        ADD, SUB,
        MUL, DIV, MOD, POW,
        AND, OR, XOR,
        SHR, SHL,
        LT, GT, EQ, LE, GE, NE
    };
    
    // Left and right _must_ have the same type. Use implicitConvert above.
    explicit BinaryOp(int op, Expression *left, Expression *right) :
            Expression(left->getType()), op(op), left(left), right(right) {
        
        assert(*left->getType() == *right->getType() &&
            "types MUST be equal for binary op");
    }
    
    explicit BinaryOp(int op, Expression *left,
            Expression *right, const Type *type) :
            Expression(type), op(op), left(left), right(right) {
            
        assert(*left->getType() == *right->getType() &&
            "types MUST be equal for binary op");
    }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int exprClass() const { return BINARYOP; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class BinaryCmpOp : public BinaryOp {
public:
    // have to pass bool type as type argument.
    explicit BinaryCmpOp(int op, Expression *left,
        Expression *right, const Type *type) :
        BinaryOp(op, left, right, type) { }
        
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class Deref : public Expression {
    Expression *expr;
    
public:
    explicit Deref(Expression *expr);
    
    virtual int exprClass() const { return DEREF; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class AddrOf : public Expression {
    Expression *expr;
    
public:
    explicit AddrOf(Expression *expr);
    
    virtual int exprClass() const { return ADDROF; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class LitConstant : public Expression {
    union {
        bool b;
        signed int i;
        unsigned int u;
        signed long long ll;
        unsigned long long ull;
        float f;
        double d;
    } value;
    std::string str;
    
    mutable llvm::Value *cached;
    mutable llvm::GlobalVariable *cached_gv;
    
public:
    explicit LitConstant(signed long long ll, const Type *ty);
    explicit LitConstant(unsigned long long ull, const Type *ty);
    explicit LitConstant(double d, const Type *ty);
    explicit LitConstant(std::string str, const Type *ty);
    
    long long getLL() const { return value.ll; }
    double getDouble() const { return value.d; }
    
    std::string getstr() const { return str; }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int exprClass() const { return LITCONST; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class Var : public Expression {
protected:
    std::string name;
    Expression *initializer;
    
public:
    explicit Var(std::string name, const Type *type) :
        Expression(type), name(name), initializer() { }
    explicit Var(std::string name, Expression *init) :
        Expression(init->getType()), name(name), initializer(init) { }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    
    const std::string getName() const { return name; }
    void setName(std::string newname) { name = newname; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual void genInitializer(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block) { }
};

class GlobalVar : public Var {
    mutable llvm::GlobalVariable *cached_gv;
    
public:
    explicit GlobalVar(const std::string name, const Type *type) :
        Var(name, type), cached_gv(0) { }
    explicit GlobalVar(const std::string name, Expression *init) :
        Var(name, init), cached_gv(0) { }
        
    virtual int exprClass() const { return GLOBALVAR; }
    
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class LocalVar : public Var {
    mutable llvm::AllocaInst *cached_alloca;
    
public:
    explicit LocalVar(const std::string name, const Type *type) :
        Var(name, type), cached_alloca(0) { }
    explicit LocalVar(const std::string name, Expression *init) :
        Var(name, init), cached_alloca(0) { }
        
    virtual int exprClass() const { return LOCALVAR; }
    
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    void createLocal(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block);
        
    virtual void genInitializer(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block);
};

class Param : public Var {
    llvm::AllocaInst *cached_alloca;
    llvm::Value *cached_val;
    
public:
    explicit Param(const std::string name, const Type *type) :
        Var(name, type), cached_alloca(0), cached_val(0) { }
        
    virtual int exprClass() const { return PARAM; }
    
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    void bindParam(llvm::Value *val, llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block);
        
    virtual void genInitializer(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *block);
};

class Indexable : public Expression {
public:
    Indexable(const Type *ty) : Expression(ty) { }
    virtual llvm::Value *getGepIndex() const { return 0; } // = 0;
};

static const Type *getRefType(Var *var) {
    if (var->getType()->typeClass() == Type::POINTER)
        return static_cast<const PointerType *>(var->getType())->getRefType();
    return static_cast<const ArrayType *>(var->getType())->getRefType();
}

class ArrayIndexer : public Indexable {
    Var *var;
    Expression **bounds;
    
public:
    // All of the given bounds are collapsed into a one dimensional array
    // accessor. The bounds array must have the same number of elements as
    // var has dimensions.
    explicit ArrayIndexer(Var *var, Expression **bounds) :
        Indexable(getRefType(var)),
        var(var),
        bounds(bounds) { }
    virtual ~ArrayIndexer();
    
    virtual int exprClass() const { return ARRAYINDEX; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    
    //virtual llvm::Value *getGepIndex() const;
};

class StructAccessor : public Indexable {
    Var *var;
    unsigned index;
    
public:
    explicit StructAccessor(Var *var, unsigned index) :
        Indexable(getRefType(var)),
        var(var),
        index(index) { }
        
    virtual int exprClass() const { return STRUCTACCESS; }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
        
    //virtual llvm::Value *getGepIndex() const;
};

class CallExpr : public Expression {
    Sub *sub;
    
protected:
    const SubType *subty;
    std::vector<Expression *> exprlist;
    int expridx;
    
    explicit CallExpr(const SubType *type);
    
public:
    explicit CallExpr(Sub *sub);
        
    virtual int exprClass() const { return CALLEXPR; }
        
    bool pushExpr(Expression *expr, Component *c);
    bool hasEnoughParams() const;
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};

class PtrCallExpr : public CallExpr {
    Var *var;
public:
    explicit PtrCallExpr(Var *var) :
        CallExpr(static_cast<const SubType *>(
            static_cast<const PointerType *>(var->getType())->getRefType()
        )), var(var) { }
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
};


//==============================================================================


class InOrderNode {
public:
    InOrderNode *next;
    
    explicit InOrderNode() : next(0) { }
    virtual ~InOrderNode() = 0;
    
    //virtual void toXml(std::ostream &file, int indent) const { };
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const = 0;
};

class Label : public InOrderNode {
    const std::string name;
    
public:
    explicit Label(const std::string name) : name(name) { }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    
    const std::string getName() const { return name; }
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
    const llvm::BasicBlock *getLlvmBasicBlock() const;
};

class CallStmt : public InOrderNode {
public:
    CallExpr *ce;
    explicit CallStmt(CallExpr *expr) : ce(expr) { }
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class Goto : public InOrderNode {
    Label *label;
    
public:
    explicit Goto(Label *label) : label(label) { }
    
    //virtual void toXml(std::ostream &file, int indent) const;
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class Assign : public InOrderNode {
    Expression *left;
    Expression *right;
    
public:
    explicit Assign(Expression *left, Expression *right) :
        left(left), right(right) { }
        
    //virtual void toXml(std::ostream &file, int indent) const;
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class Return : public InOrderNode {
    Expression *expr;
    
public:
    explicit Return() : expr() { }
    explicit Return(Expression *expr) : expr(expr) { }
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class Container {
    Container *parent;
    
public:
    explicit Container() : parent(0) { }
    explicit Container(Container *p) : parent(p) { }
    virtual ~Container() { }
    
    virtual void addStatement(InOrderNode *node) = 0;
    Container *getParent() { return parent; }
};

class IfBlock : public Container, public InOrderNode {
    Expression *expr;
    InOrderNode *first, *last;
    InOrderNode *else_first, *else_last;
    
    // Else if blocks are stored as an else { if(...) { } } type of thing.
    bool in_else;
    IfBlock *nested;
    
public:
    explicit IfBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first(0),
        else_first(0), in_else(false), nested(0) { }
    virtual ~IfBlock();
    
    virtual void addStatement(InOrderNode *node);
    bool enterElse();
    bool enterElseIf(IfBlock *elseif);
    IfBlock *getCurrentElseIf() { return nested ? nested : this; }
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class SelectBlock;
class CaseBlock : public Container {
    Expression *expr;
    friend class SelectBlock;
    
    InOrderNode *first, *last;
    
public:
    CaseBlock *next;
    
    explicit CaseBlock(Container *parent) :
        Container(parent), expr(), first(0), next(0) { }
    explicit CaseBlock(Container *parent, Expression *expr) :
        Container(parent), expr(expr), first(0), next(0) { }
    ~CaseBlock();
    
    virtual void addStatement(InOrderNode *node);
};

class SelectBlock : public Container, public InOrderNode {
    Expression *expr;
    CaseBlock *first_case, *last_case;
    
    bool has_nonconst_case;
    bool in_default; // Not necessary to initialize
    
    int nr_of_cases;
    
    // If all the cases had constant expressions, we can generate an LLVM
    // switch instruction. Otherwise, we have to generate branches like an if
    // block with a bunch of elseifs.
    bool genLlvmSwitch(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const;
    bool genLlvmIf(llvm::LLVMContext &context, llvm::Module &mod,
        llvm::BasicBlock *&block) const;
    
public:
    explicit SelectBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first_case(0),
        last_case(0), has_nonconst_case(false), nr_of_cases(0) { }
    virtual ~SelectBlock();
    
    virtual void addStatement(InOrderNode *node);
    bool enterCase(CaseBlock *c);
    bool enterDefault(CaseBlock *c);
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class ForBlock : public Container, public InOrderNode {
    Var *var;
    Expression *init;
    Expression *final;
    Expression *step;
    int cmpop;
    
    InOrderNode *first, *last;
    
public:
    explicit ForBlock(Container *parent,
            Var *var,
            Expression *init,
            Expression *final,
            Expression *step,
            int cmpop) :
        Container(parent), InOrderNode(), var(var), init(init), final(final),
        step(step), cmpop(cmpop), first(0) { }
    virtual ~ForBlock();
    
    virtual void addStatement(InOrderNode *node);
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class WhileBlock : public Container, public InOrderNode {
    Expression *expr;
    InOrderNode *first, *last;
    
public:
    explicit WhileBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first(0) { }
    virtual ~WhileBlock();
    
    virtual void addStatement(InOrderNode *node);
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class DoBlock : public Container, public InOrderNode {
    Expression *expr;
    InOrderNode *first, *last;
    
public:
    explicit DoBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first(0) { }
    virtual ~DoBlock();
    
    virtual void addStatement(InOrderNode *node);
    
    virtual bool genLlvm(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *&block) const;
};

class Sub : public Var, public Container {
public:
    typedef std::tr1::unordered_map<std::string, Var *> var_map;
        
    typedef std::tr1::unordered_map<std::string, Label *> label_map;
        
    typedef std::vector<Var *> var_list;
    
    // We use the list to keep params and locals in the same order
    // they were declared in, and the map for efficient lookups.
    
private:
    std::string libname;
    std::string aliasname;
    var_list varlist;
    var_map vars;
    label_map labels;
    InOrderNode *first, *last;
    Container *current_container;
    int flags;
    
    llvm::Constant *function;
    
public:
    enum {
        IMPLEMENTED = 0x1,
        COMMAND = 0x2,
        HAS_LIB = 0x4,
        HAS_ALIAS = 0x8
    };
    
    explicit Sub(const std::string name, const Type *type, int flags) :
        Var(name, type), first(0), current_container(0), flags(flags),
        function(0) { }
        
    ~Sub();
    
    //virtual void toXml(std::ostream &file, int indent) const;
    virtual int exprClass() const { return Expression::SUB; }
    
    int getFlags() const { return flags; }
    void setImplFlag() { flags |= IMPLEMENTED; }
    
    void setLib(std::string lib) { flags |= HAS_LIB; libname = lib; }
    const std::string getLib() const { return libname; }
    
    void setAlias(std::string alias) { flags |= HAS_ALIAS; aliasname = alias; }
    const std::string getAlias() const { return aliasname; }
    
    void setType(const Type *newtype) { type = newtype; }
    
    bool addParamOrLocal(const std::string name, Var *var);
    
    // returns false if there is already a param with that name.
    bool addParam(const std::string name, Param *param) {
        return addParamOrLocal(name, param);
    }
    
    // returns false if there is already a param or local with that name.
    bool addLocal(const std::string name, LocalVar *local) {
        return addParamOrLocal(name, local);
    }
    
    // gets a var from params or locals
    Var *getVar(const std::string name) const;
    Var *getVar(unsigned int index) const;
    
    bool setVarName(std::string oldname, std::string newname);
        
    bool addLabel(const std::string name);
    Label *getLabel(const std::string name) const;

    Container *getCurrentContainer() { return current_container; }
    // Requires current container == c->parent
    bool enterContainer(Container *c);
    bool exitContainer();
    
    virtual void addStatement(InOrderNode *stmt);
    
    virtual llvm::Value *getLlvmValue(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    virtual llvm::Value *getLlvmAddr(llvm::LLVMContext &context,
        llvm::Module &mod, llvm::BasicBlock *block) const;
    llvm::Constant *getLlvmFunction(llvm::LLVMContext &context,
        llvm::Module *mod);
};

} // namespace glaz

#endif        //  #ifndef AST_H

