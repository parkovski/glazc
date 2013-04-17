#include "glaz.h"
#include "ast.h"
#include "scanner.h"
#include <stdlib.h>
#include <stdio.h>

#ifdef _MSC_VER
#define strtoll _strtoi64
#define strtoull _strtoui64
#endif

using namespace glaz;

bool Component::pass2(Token *tree, Sub *sub) {
    bool success = true;
    
    while (tree) {
        switch (tree->id) {
        case TYPE:
            success &= pass2_type(tree);
            break;
            
        case DEF:
            // Pass null because variables declared outside of a sub are
            // global, not local to main.
            success &= pass2_def(tree,
                sub == implicit_main ? 0 : sub);
            break;
            
        case CONST:
        case SETID:
        case STRINGSETID:
            success &= pass2_const_or_setid(tree,
                sub == implicit_main ? 0 : sub);
            break;
            
        case LABEL:
            // Don't need to check for success because pass1 ensures that
            // every label is valid.
            pass2_label(tree, sub);
            break;
            
        case GOTO:
            success &= pass2_goto(tree, sub);
            break;
            
        case SUB: {
            assert(sub == implicit_main &&
                "pass1 should have ensured no nested subs");
            
            Token *subtree = tree->child; // Points to the ID node.
            Sub *newsub = getSub(subtree->text);
            assert(newsub && "pass1 should have declared all subs");
            
            while (subtree->id != AS) // the return type node
                subtree = subtree->next;
            subtree = subtree->next;
            assert(subtree && "subs must have at least a return statement");
            
            success &= pass2(subtree, newsub);
        }
            break;
            
        case CALL:
            success &= pass2_call_stmt(tree, sub);
            break;
            
        case LET:
            success &= pass2_assign(tree, sub);
            break;
            
        case IF:
            success &= pass2_if(tree, sub);
            break;
            
        case SELECT:
            success &= pass2_select(tree, sub);
            break;
            
        case FOR:
            success &= pass2_for(tree, sub);
            
        case WHILE:
            success &= pass2_while(tree, sub);
            break;
            
        case DO:
            success &= pass2_do(tree, sub);
            break;

        case PRINT:
        case PRINTLN:
            success &= pass2_print(tree, sub);
            break;
            
        case RETURN:
            if (sub == implicit_main) {
                printf("error: RETURN not allowed outside of SUB (line %d)\n",
                    tree->loc.first_line);
                success = false;
            } else {
                success &= pass2_return(tree, sub);
                
                // This should be the last statement in this list.
                if (tree->next) {
                    printf("warning: dead code starting at line %d will "
                        "not be emitted\n", tree->next->loc.first_line);
                }
                
                return success;
            }
            break;
            
        case END:
            if (sub == implicit_main) {
                success &= pass2_return(tree, sub);
                tree = tree->next;
                success &= pass2_after_end(tree);
                
                return success;
            }
            break;
        }
        
        if (!success) {
            printf("ast generation failed. aborting...\n");
            return false;
        }
        tree = tree->next;
    }
    
    return success;
}

bool Component::pass2_after_end(Token *tree) {
    bool printed_warning = false;
    
    while (tree) {
        if (tree->id != SUB) {
            if (!printed_warning) {
                printed_warning = true;
                printf("warning: no code after end (besides SUBs) will be "
                    "emitted (line %d)\n", tree->loc.first_line);
            }
        } else {
            Token *subtree = tree->child; // Points to the ID node.
            Sub *newsub = getSub(subtree->text);
            assert(newsub && "pass1 should have declared all subs");
            
            while (subtree->id != AS) // the return type node
                subtree = subtree->next;
            subtree = subtree->next;
            assert(subtree && "subs must have at least a return statement");
            
            if (!pass2(subtree, newsub)) {
                return false;
            }
        }
        
        tree = tree->next;
    }
    
    return true;
}

bool Component::pass2_type(Token *tree) {
    assert(tree->id == TYPE && "wrong node type in pass2_type");
    
    // Un-constify it, since we must build the structure in here.
    Struct *st = const_cast<Struct *>(
        static_cast<const Struct *>(getType(tree->child))
    );
    assert(st && st->typeClass() == Type::STRUCT &&
        "struct should have been created during pass1");
    
    // Check if the node after the name and optional alignment specifier
    // exists. If not, the body was empty, which is illegal.
    if (!tree->child->next ||
            (tree->child->next->id == ICON && !tree->child->next->next)) {
        printf("error: type '%s' not allowed to be empty (line %d)\n",
            tree->child->text, tree->child->loc.first_line);
        return false;
    }
    
    tree = tree->child->next;
    
    if (tree->id == ICON) // alignment specifier
        tree = tree->next;
    
    while (tree) {
        assert(tree->id == DEF && "only DEF nodes allowed in type");
        
        const Type *type = getType(tree->child);
        
        // Check if either 1) the type doesn't exist, or 2) it's a structure
        // that's not implemented yet. Since this could lead to a recursive
        // structure, disallow it. Note: pointer types should be allowed to
        // alias unimplemented structures.
        if (!type || (type->typeClass() == Type::STRUCT &&
                !static_cast<const Struct *>(type)->isImplemented())) {
            
            printf("error: undefined type '%s' (line %d)\n",
                tree->child->text, tree->child->loc.first_line);
            return false;
        }
        
        Token *subtree = tree->child->next;
        while (subtree) {
            std::string varname(subtree->text);
            GlobalVar *var = new GlobalVar(varname, type);
            if (!st->addVar(varname, var)) {
                printf("error: duplicate variable name '%s' (line %d)\n",
                    subtree->text, subtree->loc.first_line);
                delete var;
                return false;
            }
            subtree = subtree->next;
        }
        
        tree = tree->next;
    }
    
    st->setImplFlag();
    
    return true;
}

bool Component::pass2_def(Token *tree, Sub *sub) {
    const Type *type = getType(tree->child);
    if (!type) {
        printf("error: undefined type '%s' (line %d)\n",
            tree->child->text, tree->child->loc.first_line);
        return false;
    }
    
    tree = tree->child->next;
    while (tree) {
        assert(tree->id == ID && "expected ID node in DEF tree");
        std::string varname(tree->text);
        // A type for each individual element, since it could be an array.
        const Type *indivtype = 0;
        
        if (tree->child && tree->child->id == '[') {
            if (!tree->child->child->next) {
                // Make a 1 dimensional array
                /*
                bounds = pass2_constexpr(tree, sub);
                int iid = static_cast<IntrinsicType *>(bounds)
                    ->getIntrinsicId();
                
                if (iid > IntrinsicType::VOID && iid < IntrinsicType::FLOAT) {
                    ...
                }
                */
                indivtype = new ArrayType(type,
                    (unsigned)strtoul(tree->child->child->text, 0, 10));
            } else {
                Token *btok = tree->child->child;
                std::vector<unsigned> bounds;
                while (btok) {
                    // Copy from above...
                    bounds.push_back((unsigned)strtoul(btok->text, 0, 10));
                    btok = btok->next;
                }
                
                indivtype = new ArrayType(type, bounds);
            }
        } else {
            indivtype = type;
        }
        
        if (!indivtype) {
            printf("error: invalid array bounds on '%s' (line %d)\n",
                tree->text, tree->loc.first_line);
            return false;
        }
        
        Var *var;
        bool success;
        
        if (sub) {
            var = new LocalVar(varname, indivtype);
            success = sub->addLocal(varname, static_cast<LocalVar *>(var));
        } else {
            var = new GlobalVar(varname, indivtype);
            success = insertVar(varname, var);
        }
        
        if (!success) {
            printf("error: duplicate variable name '%s' (line %d)\n",
                tree->text, tree->loc.first_line);
            delete var;
            return false;
        }
        
        tree = tree->next;
    }
    
    return true;
}

bool
Component::pass2_const_or_setid(Token *tree, Sub *sub) {
    // These all infer their type from the initializer expression.
    
    std::string varname(tree->child->text);
    // If it's a setid, then this was a string constant. Fix it to have the '@'
    // and no quotes. We know it's a valid identifier already b/c pass1.
    if (tree->id != CONST) {
        varname[0] = '@'; // instead of '"'
        varname.resize(varname.size()-1); // chop of final '"'
    }
    
    Expression *initializer =
        pass2_expr(tree->child->next, sub);
    
    if (!initializer) {
        printf("error: expected constant expression (line %d)\n",
            tree->child->next->loc.first_line);
        return false;
    }

    Var *var;
    bool success;
    if (sub) {
        var = new LocalVar(varname, initializer);
        success = sub->addLocal(varname, static_cast<LocalVar *>(var));
    } else {
        var = new GlobalVar(varname, initializer);
        success = insertVar(varname, var);
    }
    
    if (!success) {
        printf("error: duplicate variable name '%s' (line %d)\n",
            tree->child->text, tree->child->loc.first_line);
        delete var;
        return false;
    }
    
    return true;
}

bool Component::pass2_label(Token *tree, Sub *sub) {
    Label *label = sub->getLabel(tree->child->text);
    
    assert(label && "pass1 should have added all labels");
    
    sub->addStatement(label);
    
    return true;
}

bool Component::pass2_goto(Token *tree, Sub *sub) {
    Label *label = sub->getLabel(tree->child->text);
    if (!label) {
        printf("error: undefined label '%s' (line %d)\n",
            tree->child->text, tree->child->loc.first_line);
        return false;
    }
    
    sub->addStatement(new Goto(label));
    
    return true;
}

bool Component::pass2_assign(Token *tree, Sub *sub) {
    // (LET left right)
    Expression *left = resolveVar(tree->child->text, sub);
    if (!left) {
        printf("error: undefined variable '%s' (line %d)\n",
            tree->child->text, tree->child->loc.first_line);
        return false;
    }
    
    if (tree->child->child && tree->child->child->id == '[') {
        left = pass2_arrayindex(static_cast<Var *>(left),
            tree->child->child, sub);
    }
    
    Expression *
        right(pass2_expr(tree->child->next, sub));
    
    if (!right)
        return false;
    
    if (!implicitConvert(right, left->getType(), this)) {
        printf("error: types %s and %s are incompatible "
            "(line %d)\n", left->getType()->getName().c_str(),
            right->getType()->getName().c_str(),
            tree->loc.first_line);
            
        return 0;
    }
    
    sub->addStatement(new Assign(left, right));
    
    return true;
}

bool Component::pass2_return(Token *tree, Sub *sub) {
    if (tree->child) {
        Expression *expr = pass2_expr(tree->child, sub);
        
        if (!expr)
            return false;
        
        sub->addStatement(new Return(expr));
        return true;
    } else {
        sub->addStatement(new Return());
        return true;
    }
}

// Only called for END inside a function. In main, it's handled by
// pass2_return.
bool Component::pass2_end(Token *tree, Sub *sub) {
    return false;
}

Expression *
Component::pass2_expr(const Token *tree, Sub *sub) {
    switch (tree->id) {
    case UMINUS:
        return pass2_uexpr(tree->child, UnaryOp::NEG, sub);
        
    case '+':
        return pass2_bexpr(tree->child, BinaryOp::ADD, sub);
        
    case '-':
        return pass2_bexpr(tree->child, BinaryOp::SUB, sub);
        
    case '*':
        return pass2_bexpr(tree->child, BinaryOp::MUL, sub);
        
    case '/':
        return pass2_bexpr(tree->child, BinaryOp::DIV, sub);
        
    case '%':
        return pass2_bexpr(tree->child, BinaryOp::MOD, sub);
        
    case '^':
        return pass2_bexpr(tree->child, BinaryOp::POW, sub);
        
    case '&':
        return pass2_bexpr(tree->child, BinaryOp::AND, sub);
        
    case '|':
        return pass2_bexpr(tree->child, BinaryOp::OR, sub);
        
    case XOR:
        return pass2_bexpr(tree->child, BinaryOp::XOR, sub);
        
    case RSHIFT:
        return pass2_bexpr(tree->child, BinaryOp::SHR, sub);
        
    case LSHIFT:
        return pass2_bexpr(tree->child, BinaryOp::SHL, sub);
        
    case '<':
        return pass2_cmpexpr(tree->child, BinaryOp::LT, sub);
        
    case '>':
        return pass2_cmpexpr(tree->child, BinaryOp::GT, sub);
        
    case '=':
        return pass2_cmpexpr(tree->child, BinaryOp::EQ, sub);
        
    case LEQ:
        return pass2_cmpexpr(tree->child, BinaryOp::LE, sub);
        
    case GEQ:
        return pass2_cmpexpr(tree->child, BinaryOp::GE, sub);
        
    case NEQ:
        return pass2_cmpexpr(tree->child, BinaryOp::NE, sub);
        
    case SYSVAR:
    case ID: {
        Var *var = resolveVar(tree->text, sub);
        if (!var) {
            printf("error: undefined variable %s (line %d)\n",
                tree->text, tree->loc.first_line);
        }
        
        if (tree->child && var && tree->child->id == '[') {
            return pass2_arrayindex(var, tree->child, sub);
        }
        return var;
    }
    
    case TO: {
        Expression *expr =
            pass2_expr(tree->child->next, sub);
        const Type *ty = getType(tree->child);
        
        if (!implicitConvert(expr, ty, this))
            return 0;
            
        return expr;
    }
    
    case ADDROF:
        // TODO: make sure these are things that actually have addresses.
        return new AddrOf(pass2_expr(tree->child, sub));
    
    case ICON:
    case UCON:
    case LCON:
    case ULCON:
    case FCON:
    case DCON:
    case SCON:
        return pass2_litconst(tree);
    
    case CALL:
        return pass2_call(tree, sub);
        
    default:
        printf("%d: expression unimplemented %s :(\n",
            tree->loc.first_line, tree->text);
        return 0;
    }
}

Expression *
Component::pass2_uexpr(const Token *tree, int op, Sub *sub) {
    Expression *inner = pass2_expr(tree, sub);
    if (!inner)
        return 0;
    return new UnaryOp(op, inner);
}

Expression *
Component::pass2_bexpr(const Token *tree, int op, Sub *sub) {
    Expression *left = pass2_expr(tree, sub);
    if (!left)
        return 0;
    
    Expression *right = pass2_expr(tree->next, sub);
    if (!right)
        return 0;
        
    if (!implicitConvert(left, right, this)) {
        printf("error: types %s and %s are incompatible "
            "(line %d)\n", left->getType()->getName().c_str(),
            right->getType()->getName().c_str(),
            tree->loc.first_line);
            
        return 0;
    }
    
    return new BinaryOp(op, left, right);
}

Expression *
Component::pass2_cmpexpr(const Token *tree, int op,
        Sub *sub) {
            
    Expression *left = pass2_expr(tree, sub);
    if (!left)
        return 0;
    
    Expression *right = pass2_expr(tree->next, sub);
    if (!right)
        return 0;
    
    if (!implicitConvert(left, right, this)) {
        printf("error: types %s and %s are incompatible "
            "(line %d)\n", left->getType()->getName().c_str(),
            right->getType()->getName().c_str(),
            tree->loc.first_line);
            
        return 0;
    }
        
    return new BinaryCmpOp(op, left, right, getType("bool"));
}

Expression *
Component::pass2_litconst(const Token *tree) {
    switch (tree->id) {
    case ICON:
        return new LitConstant(strtoll(tree->text, 0, 10), getType("int"));
        
    case UCON:
        return new LitConstant(strtoull(tree->text, 0, 10), getType("uint"));
        
    case LCON:
        return new LitConstant(strtoll(tree->text, 0, 10), getType("int64"));
        
    case ULCON:
        return new LitConstant(strtoull(tree->text, 0, 10), getType("uint64"));
        
    case FCON:
        return new LitConstant(strtod(tree->text, 0), getType("float"));
        
    case DCON:
        return new LitConstant(strtod(tree->text, 0), getType("double"));
            
    case SCON: {
        std::string text(tree->text);
        return new LitConstant(text,
            new ArrayType(getType("char"), text.length())
        );
    }
        
    default:
        assert(0 && "invalid constant type");
        return 0;
    }
}

Expression *
Component::pass2_call(const Token *tree, Sub *sub) {
    assert(tree->id == CALL && "must have call node");
    
    const Token *idnode = tree->child;
    CallExpr *call;
    Var *var = getVar(idnode->text);
    
    if (!var) {
        printf("error: undeclared sub '%s' (line %d)\n",
            idnode->text, idnode->loc.first_line);
        return 0;
    }
    
    switch (var->getType()->typeClass()) {
    case Type::SUBTYPE:
        call = new CallExpr(static_cast<Sub *>(var));
        break;
        
    case Type::POINTER:
        // Can only call if it points to a SubType.
        if (static_cast<const PointerType *>(var->getType())
                ->getRefType()->typeClass() == Type::SUBTYPE) {
            
            call = new PtrCallExpr(var);
            break;
        }
        // Otherwise fall through.
        
    default:
        printf("error: variable %s not of callable type (line %d)\n",
            idnode->text, idnode->loc.first_line);
        return 0;
    }
        
    tree = tree->child->next;
    while (tree) {
        Expression *param = pass2_expr(tree, sub);
        if (!param)
            return 0;
        
        if (!call->pushExpr(param, this)) {
            printf("error: param type mismatch or too many params "
                "for call to '%s' (line %d)\n",
                idnode->text, idnode->loc.first_line);
            return 0;
        }
        
        tree = tree->next;
    }
    
    if (!call->hasEnoughParams()) {
        printf("error: not enough params for call to '%s' (line %d)\n",
            idnode->text, idnode->loc.first_line);
    }
    
    return call;
}

bool
Component::pass2_call_stmt(const Token *tree, Sub *sub) {
    Expression *ex = pass2_call(tree, sub);
    if (!ex)
        return false;
        
    sub->addStatement(new CallStmt(static_cast<CallExpr *>(ex)));
        
    return true;
}

Expression *
Component::pass2_cast(const Token *tree, Sub *sub) {
    return 0;
}

Expression *
Component::pass2_addrof(const Token *tree, Sub *sub) {
    return 0;
}

Expression *
Component::pass2_deref(const Token *tree, Sub *sub) {
    return 0;
}

Expression *
Component::pass2_structmember(const Token *tree, Sub *sub) {

    return 0;
}

Expression *
Component::pass2_arrayindex(Var *var, const Token *tree, Sub *sub) {
    unsigned nr_of_bounds =
        static_cast<const ArrayType *>(var->getType())->getNumBounds();
    Expression **bounds = new Expression *[nr_of_bounds];
    Token *bound = tree->child;
    for (unsigned i = 0; i < nr_of_bounds; ++i) {
        bounds[i] = pass2_expr(bound, sub);
        bound = bound->next;
    }
    return new ArrayIndexer(var, bounds);
}

bool
Component::pass2_if(Token *&tree, Sub *sub) {
    Expression *expr = pass2_expr(tree->child, sub);
    bool success = true;
        
    if (!expr) {
        success = false;
        // Fake an expression for error recovery, otherwise all the elseif
        // and else branches will cause tons of unneeded errors.
        expr = new LitConstant(0LL, getType("bool"));
    }
    
    //assert(*expr->getType() == *getType("bool") && "implement coersion...");
    if (!implicitConvert(expr, getType("bool"), this)) {
        printf("error: no implicit conversion from %s to bool available "
            "(line %d)\n", expr->getType()->getName().c_str(),
            tree->loc.first_line);
        success = false;
    }
    
    IfBlock *block = new IfBlock(sub->getCurrentContainer(), expr);
    sub->addStatement(block);
    sub->enterContainer(block);
    success &= pass2(tree->child->next, sub);
    
    while (tree->next && tree->next->id == ELSEIF) {
        tree = tree->next;
        
        expr = pass2_expr(tree->child, sub);
        if (!expr) {
            success = false;
            expr = new LitConstant(0LL, getType("bool"));
        }
        
        //assert(*expr->getType() == *getType("bool") && "implement coersion...");
        if (!implicitConvert(expr, getType("bool"), this)) {
            printf("error: no implicit conversion from %s to bool available "
                "(line %d)\n", expr->getType()->getName().c_str(),
                tree->loc.first_line);
            success = false;
        }
        
        block->enterElseIf(new IfBlock(block->getCurrentElseIf(), expr));
        success &= pass2(tree->child->next, sub);
    }
    
    if (tree->next && tree->next->id == ELSE) {
        tree = tree->next;
        
        block->enterElse();
        
        success &= pass2(tree->child, sub);
    }
    
    sub->exitContainer();
    return success;
}

bool
Component::pass2_select(Token *&tree, Sub *sub) {
    Expression *expr = pass2_expr(tree->child, sub);
    bool success = true;
    
    if (!expr) {
        success = false;
        // Fake an expression for error recovery, otherwise all the elseif
        // and else branches will cause tons of unneeded errors.
        expr = new LitConstant(0LL, getType("int"));
    }
    
    SelectBlock *block = new SelectBlock(sub->getCurrentContainer(), expr);
    sub->addStatement(block);
    sub->enterContainer(block);

    while (tree->next) {
        CaseBlock *cb;
        switch (tree->next->id) {
        case CASE: {
            tree = tree->next;
            Expression *caseexpr = pass2_expr(tree->child, sub);
            if (!caseexpr) {
                success = false;
                // Error recovery, same as above.
                caseexpr = new LitConstant(0LL, getType("int"));
            }
            
            assert(*caseexpr->getType() == *expr->getType() &&
                "implement coersion...");
            
            cb = new CaseBlock(block, caseexpr);
            block->enterCase(cb);
        }
            success &= pass2(tree->child->next, sub);
            break;
            
        case DEFAULT:
            tree = tree->next;
            cb = new CaseBlock(block);
            block->enterDefault(cb);
            success &= pass2(tree->child, sub);
            break;
            
        default:
            goto done_with_select;
        }
    }
done_with_select:

    sub->exitContainer();
    return success;
}

bool Component::pass2_for(Token *&tree, Sub *sub) {
    // None of the expressions <init>, <end>, <step> is evaluated more than
    // once. Also, they all must be of an integral type.
    
    bool success = true;
    Var *var = resolveVar(tree->child->text, sub);
    if (!var) {
        printf("error: undefined variable %s (line %d)\n",
            tree->child->text, tree->child->loc.first_line);
            
        // Don't try any error recovery here, because the variable is probably
        // used in the loop, and would cause a bunch more errors anyways.
        return false;
    }
    
    Expression *init = pass2_expr(tree->child->next, sub);
    if (!init) {
        success = false;
        init = new LitConstant(0LL, var->getType());
    }
        
    Expression *end = pass2_expr(tree->child->next->next, sub);
    if (!end) {
        success = false;
        end = new LitConstant(0LL, var->getType());
    }
        
    Token *node = tree->child->next->next->next;
    Expression *step;
    
    // The STEP part is optional, and defaults to 1 if not present.
    if (node && node->id == STEP) {
        step = pass2_expr(node->child, sub);
        if (!step) {
            success = false;
            step = new LitConstant(1LL, var->getType());
        }
        node = node->next;
    } else {
        step = new LitConstant(1LL, var->getType());
    }
    
    // If we don't know whether to test for greater or less than, we will
    // have to check at each iteration of the loop. Note that since step
    // doesn't change, in the future this could be optimized into a pointer
    // to a label and an indirect branch.
    int cmpop = BinaryOp::INVALID;
    if (step->exprClass() == Expression::LITCONST) {
        LitConstant *lit = static_cast<LitConstant *>(step);
        if (lit->getLL() < 0)
            cmpop = BinaryOp::GE;
        else
            cmpop = BinaryOp::LE;
    }
    
    ForBlock *block = new ForBlock(sub->getCurrentContainer(),
        var, init, end, step, cmpop);
    sub->addStatement(block);
    sub->enterContainer(block);
    
    success &= pass2(node, sub);
    
    sub->exitContainer();
    
    return success;
}

bool Component::pass2_while(Token *&tree, Sub *sub) {
    Expression *expr = pass2_expr(tree->child, sub);
    bool success = true;
    
    if (!expr) {
        success = false;
        expr = new LitConstant(0LL, getType("bool"));
    }
    
    if (!implicitConvert(expr, getType("bool"), this)) {
        printf("error: no implicit conversion from %s to bool available "
            "(line %d)\n", expr->getType()->getName().c_str(),
            tree->loc.first_line);
        success = false;
    }
    
    WhileBlock *block = new WhileBlock(sub->getCurrentContainer(), expr);
    sub->addStatement(block);
    sub->enterContainer(block);
    
    success &= pass2(tree->child->next, sub);
    
    sub->exitContainer();
    
    return success;
}

bool Component::pass2_do(Token *&tree, Sub *sub) {
    Expression *expr = pass2_expr(tree->child->child, sub);
    bool success = true;
    
    if (!expr) {
        success = false;
        expr = new LitConstant(0LL, getType("bool"));
    }
    
    if (!implicitConvert(expr, getType("bool"), this)) {
        printf("error: no implicit conversion from %s to bool available "
            "(line %d)\n", expr->getType()->getName().c_str(),
            tree->loc.first_line);
        success = false;
    }
    
    DoBlock *block = new DoBlock(sub->getCurrentContainer(), expr);
    sub->addStatement(block);
    sub->enterContainer(block);
    
    success &= pass2(tree->child->next, sub);
    
    sub->exitContainer();
    
    return success;
}

bool Component::pass2_print(Token *&tree, Sub *sub) {
    // (PRINT|PRINTLN (expr expr...))
    // Each expr becomes its own _GB_print* call
    bool println = tree->id == PRINTLN;
    Token *t = tree->child;
    while (t) {
        Expression *expr = pass2_expr(t, sub);
        if (!expr) {
            return false;
        }

        char *subname = 0;
        const Type *ty = expr->getType();
        if (ty->typeClass() == Type::INTRINSIC) {
            int iid = static_cast<const IntrinsicType *>(ty)->getIntrinsicId();
            switch (iid) {
            case IntrinsicType::BOOL:
                subname = "_GB_printBool";
                break;
            case IntrinsicType::CHAR:
                subname = "_GB_printChar";
                break;
            case IntrinsicType::INT:
                subname = "_GB_printInt";
                break;
            case IntrinsicType::UINT:
                subname = "_GB_printUint";
                break;
            case IntrinsicType::INT64:
                subname = "_GB_printInt64";
                break;
            case IntrinsicType::UINT64:
                subname = "_GB_printUint64";
                break;
            case IntrinsicType::FLOAT:
            case IntrinsicType::DOUBLE:
                subname = "_GB_printDouble";
                break;
            default:
                // These should be uncommon enough that we'll just convert them to int.
                assert(iid == IntrinsicType::SCHAR
                       || iid == IntrinsicType::WORD
                       || iid == IntrinsicType::SWORD);
                implicitConvert(expr, getType("int"), this);
                subname = "_GB_printInt";
                break;
            }
        } else if (ty->typeClass() == Type::ARRAY) {
            auto at = static_cast<const ArrayType *>(ty);
            if (*at->getRefType() != *getType("char")) {
                printf("don't know how to print type %s\n", ty->getName().c_str());
                return false;
            }
            implicitConvert(expr, getType("char")->getPtrType(), this);
            subname = "_GB_printStr";
        } else if (ty->typeClass() == Type::POINTER) {
            auto pt = static_cast<const PointerType *>(ty);
            if (*pt->getRefType() == *getType("char")) {
                subname = "_GB_printStr";
            } else {
                switch (sizeof(void *)) {
                case 4:
                    subname = "_GB_printUint";
                    break;
                case 8:
                    subname = "_GB_printUint64";
                    break;
                default:
                    assert(false && "not 32 or 64 bit?");
                    return false;
                }
            }
        } else {
            printf("don't know how to print type %s\n", ty->getName().c_str());
            return false;
        }

        Sub *printSub = getSub(subname);
        if (!printSub) {
            printf("can't find subroutine %s\n", subname);
            return false;
        }

        CallExpr *ce = new CallExpr(printSub);
        ce->pushExpr(expr, this);
        sub->addStatement(new CallStmt(ce));

        t = t->next;
    }

    if (println) {
        Sub *printlnSub = getSub("_GB_printLine");
        if (!printlnSub) {
            printf("can't find subroutine _GB_printLine\n");
            return false;
        }
        sub->addStatement(new CallStmt(new CallExpr(printlnSub)));
    }

    return true;
}

const Type *
Component::getType(const Token *tok) const {
    if (tok->id == '*')
        return getType(tok->child)->getPtrType();
    return getType(tok->text);
}

Var *
Component::resolveVar(const std::string name, const Sub *sub) const {
    // Try to find in locals, then params, then globals. Return null if not
    // found in any of those.
    
    Var *var;
    
    if (sub != implicit_main) {
        var = sub->getVar(name);
        if (var)
            return var;
    }
    
    return this->getVar(name);
}

