#ifndef AST_H
#define AST_H

#include "ast-expr.h"

#include <cassert>
#include <unordered_map>
#include <string>
#include <vector>

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
    
    typedef std::unordered_map<std::string, const Type *> type_map;
        
private:
    // This includes all vars, subs, consts, setids, etc.
    var_map vars;
    type_map types;
    // For convenience, to avoid lookups.
    const Type *void_type;
    const SubType *void_sub_type;
    
    Sub *implicit_main;
    
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
    bool pass2_print(Token *&tree, Sub *sub);
    
    const Type *getType(const Token *tok) const;
    Var *resolveVar(const std::string &name, const Sub *sub) const;
    
public:
    explicit Component();
    ~Component();
    
    static Component *fromTree(Token *tree, bool del = true);
    
    Sub *getMain() { return implicit_main; }
    
    Sub *getSub(const std::string &name) const;
    Sub *insertSub(const std::string &name, int flags);
    
    const Type *getType(const std::string &name) const;
    bool insertType(const std::string &name, const Type *type);
    
    Var *getVar(const std::string &name) const;
    bool insertVar(const std::string &name, Var *var);

    std::string toString() const;
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
    
    Type() : ptrtome(nullptr) { }
    virtual ~Type() { }
    
    // Returns which class of type it is (I know, awkward wording). E.g.,
    // intrinsic, structure, pointer, array, etc.
    virtual int typeClass() const = 0;
    virtual bool operator==(const Type &rhs) const = 0;
    virtual std::string getName() const = 0;
    bool operator!=(const Type &rhs) const { return !(*this == rhs); }
    bool isVoid() const; // For easy test of void or not
    const PointerType *getPtrType() const;

    virtual std::string toString() const = 0;
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
    
public:
    explicit IntrinsicType(IntrinsicId which);
    
    virtual int typeClass() const { return INTRINSIC; }
    virtual bool operator==(const Type &rhs) const;
    virtual std::string getName() const { return name; }
    IntrinsicId getIntrinsicId() const { return which; }

    virtual std::string toString() const override;
};

class Struct : public Type {
public:
    typedef std::vector<std::pair<std::string, Var *>> var_list;
    
private:
    const std::string name;
    // 0 means to use the default alignment.
    const unsigned alignment;
    var_list vars;
    bool is_implemented; // for forward references
    
public:
    explicit Struct(const std::string &name) :
        Type(),
        name(name),
        alignment(0),
        is_implemented(false) { }
        
    explicit Struct(const std::string &name, unsigned align) :
        Type(),
        name(name),
        alignment(align),
        is_implemented(false) { }
    
    virtual ~Struct();
    
    virtual int typeClass() const { return STRUCT; }
    virtual bool operator==(const Type &rhs) const;
    virtual std::string getName() const { return name; }
    
    void setImplFlag() { is_implemented = true; }
    bool isImplemented() const { return is_implemented; }
    bool addVar(const std::string &varName, Var *var);

    virtual std::string toString() const override;
};

class PointerType : public Type {
    const Type *referenced;
    const std::string name;
    
    friend const PointerType *Type::getPtrType() const;
    
    explicit PointerType(const Type *ty) :
        Type(),
        referenced(ty),
        name(ty->isVoid() ? "pointer" : ty->getName() + "*") { }
    
public:
    virtual int typeClass() const { return POINTER; }
    virtual bool operator==(const Type &rhs) const;
    virtual std::string getName() const { return name; }
    
    const Type *getRefType() const { return referenced; }

    virtual std::string toString() const override;
};

class ArrayType : public Type {
    const Type *referenced;
    const std::string name;
    
    std::vector<unsigned> bounds;
    unsigned sd_bounds;
    
public:
    explicit ArrayType(const Type *ty, unsigned bounds);
    explicit ArrayType(const Type *ty, const std::vector<unsigned> &bounds);
            
    virtual int typeClass() const { return ARRAY; }
    virtual bool operator==(const Type &rhs) const;
    virtual std::string getName() const { return name; }
    
    const Type *getRefType() const { return referenced; }
    
    unsigned getNumBounds() const { return bounds.size(); }
    unsigned getBound(unsigned n) const { return bounds[n]; }

    virtual std::string toString() const override;
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
    
public:
    explicit SubType(const std::string &name) :
        Type(),
        rtype(nullptr),
        param_types(),
        name(name),
        flags(0) { }
    
    virtual int typeClass() const { return SUBTYPE; }
    virtual bool operator==(const Type &rhs) const;
    
    // Note: name is finalized when you call this, so you should not add any
    // more parameters or set the return type after calling this.
    virtual std::string getName() const;
    
    int getFlags() const { return flags; }
    void addFlags(int fl) { flags |= fl; }
    
    void setReturnType(const Type *newRtype) {
        this->rtype = newRtype;
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

    virtual std::string toString() const override;
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
    
    virtual int exprClass() const { return UNARYOP; }

    virtual std::string toString() const override;
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
    
    virtual int exprClass() const { return BINARYOP; }

    virtual std::string toString() const override;
};

class BinaryCmpOp : public BinaryOp {
public:
    // have to pass bool type as type argument.
    explicit BinaryCmpOp(int op, Expression *left,
        Expression *right, const Type *type) :
        BinaryOp(op, left, right, type) { }
};

class Deref : public Expression {
    Expression *expr;
    
public:
    explicit Deref(Expression *expr);
    
    virtual int exprClass() const { return DEREF; }

    virtual std::string toString() const override;
};

class AddrOf : public Expression {
    Expression *expr;
    
public:
    explicit AddrOf(Expression *expr);
    
    virtual int exprClass() const { return ADDROF; }

    virtual std::string toString() const override;
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
    
public:
    explicit LitConstant(signed long long ll, const Type *ty);
    explicit LitConstant(unsigned long long ull, const Type *ty);
    explicit LitConstant(double d, const Type *ty);
    explicit LitConstant(const std::string &str, const Type *ty);
    
    long long getLL() const { return value.ll; }
    double getDouble() const { return value.d; }
    
    std::string getstr() const { return str; }
    
    virtual int exprClass() const { return LITCONST; }

    virtual std::string toString() const override;
};

class Var : public Expression {
protected:
    std::string name;
    Expression *initializer;
    
public:
    explicit Var(const std::string &name, const Type *type) :
        Expression(type), name(name), initializer() { }
    explicit Var(const std::string &name, Expression *init) :
        Expression(init->getType()), name(name), initializer(init) { }
        
    std::string getName() const { return name; }
    void setName(const std::string &newname) { name = newname; }

    virtual std::string toString() const override;
};

class GlobalVar : public Var {
public:
    explicit GlobalVar(const std::string &name, const Type *type) :
        Var(name, type) { }
    explicit GlobalVar(const std::string &name, Expression *init) :
        Var(name, init) { }
        
    virtual int exprClass() const { return GLOBALVAR; }
};

class LocalVar : public Var {
public:
    explicit LocalVar(const std::string &name, const Type *type) :
        Var(name, type) { }
    explicit LocalVar(const std::string &name, Expression *init) :
        Var(name, init) { }
        
    virtual int exprClass() const { return LOCALVAR; }
};

class Param : public Var {
public:
    explicit Param(const std::string &name, const Type *type) :
        Var(name, type) { }
        
    virtual int exprClass() const { return PARAM; }
};

class Indexable : public Expression {
public:
    Indexable(const Type *ty) : Expression(ty) { }
};

static const Type *getRefType(Var *var) {
    if (var->getType()->typeClass() == Type::POINTER)
        return static_cast<const PointerType *>(var->getType())->getRefType();
    if (var->getType()->typeClass() == Type::ARRAY)
        return static_cast<const ArrayType *>(var->getType())->getRefType();
    assert(0 && "can't get referring type for non-pointer or array type!");
    return nullptr;
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

    virtual std::string toString() const override;
};

class StructAccessor : public Indexable {
    Var *var;
    unsigned index;
    
public:
    explicit StructAccessor(Var *var, unsigned index) :
        Indexable(getRefType(var)),
        var(var),
        index(index) { }
    virtual ~StructAccessor();
        
    virtual int exprClass() const { return STRUCTACCESS; }

    virtual std::string toString() const override;
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

    virtual std::string toString() const override;
};

class PtrCallExpr : public CallExpr {
    Var *var;
public:
    explicit PtrCallExpr(Var *var) :
        CallExpr(static_cast<const SubType *>(
            static_cast<const PointerType *>(var->getType())->getRefType()
        )), var(var) { }

    virtual std::string toString() const override;
};


//==============================================================================


class InOrderNode {
public:
    InOrderNode *next;
    
    explicit InOrderNode() : next(0) { }
    virtual ~InOrderNode() = 0;

    virtual std::string toString() const = 0;
};

class Label : public InOrderNode {
    const std::string name;
    
public:
    explicit Label(const std::string &name) : name(name) { }
        
    std::string getName() const { return name; }

    virtual std::string toString() const override;
};

class CallStmt : public InOrderNode {
public:
    CallExpr *ce;
    explicit CallStmt(CallExpr *expr) : ce(expr) { }

    virtual std::string toString() const override;
};

class Goto : public InOrderNode {
    Label *label;
    
public:
    explicit Goto(Label *label) : label(label) { }

    virtual std::string toString() const override;
};

class Assign : public InOrderNode {
    Expression *left;
    Expression *right;
    
public:
    explicit Assign(Expression *left, Expression *right) :
        left(left), right(right) { }

    virtual std::string toString() const override;
};

class Return : public InOrderNode {
    Expression *expr;
    
public:
    explicit Return() : expr(nullptr) { }
    explicit Return(Expression *expr) : expr(expr) { }

    virtual std::string toString() const override;
};

class Container {
    Container *parent;
    
public:
    explicit Container() : parent(nullptr) { }
    explicit Container(Container *p) : parent(p) { }
    virtual ~Container() { }
    
    virtual void addStatement(InOrderNode *node) = 0;
    Container *getParent() { return parent; }

    virtual std::string toString() const = 0;
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
        Container(parent), InOrderNode(), expr(expr), first(nullptr),
        else_first(nullptr), in_else(false), nested(nullptr) { }
    virtual ~IfBlock();
    
    virtual void addStatement(InOrderNode *node);
    bool enterElse();
    bool enterElseIf(IfBlock *elseif);
    IfBlock *getCurrentElseIf() { return nested ? nested : this; }

    virtual std::string toString() const override;
};

class SelectBlock;
class CaseBlock : public Container {
    Expression *expr;
    friend class SelectBlock;
    
    InOrderNode *first, *last;
    
public:
    CaseBlock *next;
    
    explicit CaseBlock(Container *parent) :
        Container(parent), expr(), first(nullptr), next(nullptr) { }
    explicit CaseBlock(Container *parent, Expression *expr) :
        Container(parent), expr(expr), first(nullptr), next(nullptr) { }
    ~CaseBlock();
    
    virtual void addStatement(InOrderNode *node);

    virtual std::string toString() const override;
};

class SelectBlock : public Container, public InOrderNode {
    Expression *expr;
    CaseBlock *first_case, *last_case;
    
    bool has_nonconst_case;
    bool in_default; // Not necessary to initialize
    
    int nr_of_cases;
    
public:
    explicit SelectBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first_case(nullptr),
        last_case(nullptr), has_nonconst_case(false), nr_of_cases(0) { }
    virtual ~SelectBlock();
    
    virtual void addStatement(InOrderNode *node);
    bool enterCase(CaseBlock *c);
    bool enterDefault(CaseBlock *c);

    virtual std::string toString() const override;
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
        step(step), cmpop(cmpop), first(nullptr) { }
    virtual ~ForBlock();
    
    virtual void addStatement(InOrderNode *node);

    virtual std::string toString() const override;
};

class WhileBlock : public Container, public InOrderNode {
    Expression *expr;
    InOrderNode *first, *last;
    
public:
    explicit WhileBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first(nullptr) { }
    virtual ~WhileBlock();
    
    virtual void addStatement(InOrderNode *node);

    virtual std::string toString() const override;
};

class DoBlock : public Container, public InOrderNode {
    Expression *expr;
    InOrderNode *first, *last;
    
public:
    explicit DoBlock(Container *parent, Expression *expr) :
        Container(parent), InOrderNode(), expr(expr), first(nullptr) { }
    virtual ~DoBlock();
    
    virtual void addStatement(InOrderNode *node);

    virtual std::string toString() const override;
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
    
public:
    enum {
        IMPLEMENTED = 0x1,
        COMMAND = 0x2,
        HAS_LIB = 0x4,
        HAS_ALIAS = 0x8
    };
    
    explicit Sub(const std::string &name, const SubType *type, int flags) :
        Var(name, type), first(nullptr), current_container(nullptr), flags(flags) { }
        
    ~Sub();
    
    virtual int exprClass() const { return Expression::SUB; }
    
    int getFlags() const { return flags; }
    void setImplFlag() { flags |= IMPLEMENTED; }
    
    void setLib(const std::string &lib) { flags |= HAS_LIB; libname = lib; }
    const std::string getLib() const { return libname; }
    
    void setAlias(const std::string &alias) { flags |= HAS_ALIAS; aliasname = alias; }
    const std::string getAlias() const { return aliasname; }
    
    void setType(const SubType *newtype) { type = newtype; }
    const SubType *getSubType() const {
        assert(type->typeClass() == Type::SUBTYPE);
        return static_cast<const SubType *>(type);
    }
    
    bool addParamOrLocal(const std::string &varName, Var *var);
    
    // returns false if there is already a param with that name.
    bool addParam(const std::string &varName, Param *param) {
        return addParamOrLocal(varName, param);
    }
    
    // returns false if there is already a param or local with that name.
    bool addLocal(const std::string &varName, LocalVar *local) {
        return addParamOrLocal(varName, local);
    }
    
    // gets a var from params or locals
    Var *getVar(const std::string &varName) const;
    Var *getVar(unsigned int index) const;
    
    bool setVarName(const std::string &oldname, const std::string &newname);
        
    bool addLabel(const std::string &labelName);
    Label *getLabel(const std::string &labelName) const;

    Container *getCurrentContainer() { return current_container; }
    // Requires current container == c->parent
    bool enterContainer(Container *c);
    bool exitContainer();
    
    virtual void addStatement(InOrderNode *stmt);

    virtual std::string toString() const override;
};

} // namespace glaz

#endif        //  #ifndef AST_H

