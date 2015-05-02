#include "glaz.h"
#include "ast.h"
#include "scanner.h"
#include <stdio.h>

using namespace glaz;

bool Component::pass1(Token *tree) {
    bool found_end = false;
    bool found_this_branch;
    bool success = true;
    
    Sub *mainsub = getMain();
    
    while (tree) {
        found_this_branch = false;
        switch (tree->id) {
        case IF:
            success &= pass1_if(tree, mainsub, found_this_branch);
            break;
            
        case SELECT:
            success &= pass1_select(tree, mainsub, found_this_branch);
            break;
            
        case FOR:
            success &= pass1_for(tree, mainsub, found_this_branch);
            break;
            
        case WHILE:
        case DO:
            success &= pass1_do_or_while(tree, mainsub, found_this_branch);
            break;
            
        case SUB:
            success &= pass1_sub(tree);
            break;
            
        case DECLARE:
            success &= pass1_declare(tree);
            break;
            
        case LABEL:
            if (found_end)
                success &= pass1_sub(tree);
            else
                success &= pass1_label(tree, mainsub);
            break;
            
        case TYPE:
            success &= pass1_type(tree);
            break;
            
        case END:
            found_end = true;
            break;
        }
        
        found_end |= found_this_branch;
        tree = tree->next;
    }
    
    return success;
}

bool Component::pass1_in_control_stmt(Token *&stmt, Sub *sub, bool &foundRet) {
    bool found_this_branch;

    while (stmt) {
        found_this_branch = false;
        switch (stmt->id) {
        case IF:
            if (!pass1_if(stmt, sub, found_this_branch))
                return false;
            break;
            
        case SELECT:
            if (!pass1_select(stmt, sub, found_this_branch))
                return false;
            break;
            
        case FOR:
            if (!pass1_for(stmt, sub, found_this_branch))
                return false;
            break;
            
        case WHILE:
        case DO:
            if (!pass1_do_or_while(stmt, sub, found_this_branch))
                return false;
            break;
            
        case RETURN:
            if (sub != implicit_main)
                foundRet = true;
            break;
            
        case END:
            foundRet = true;
            break;
            
        case SUB:
            printf("error: SUB not allowed in control statement (line %d)\n",
                stmt->loc.first_line);
            return false;
            
        case DECLARE:
            if (!pass1_declare(stmt))
                return false;
            break;
            
        case LABEL:
            if (!pass1_label(stmt, sub))
                return false;
            break;
            
        case TYPE:
            if (!pass1_type(stmt))
                return false;
            break;
        }
        
        foundRet |= found_this_branch;
        stmt = stmt->next;
    }
    
    return true;
}

bool Component::pass1_if(Token *&tree, Sub *sub, bool &foundRet) {
    bool found_this_branch;
    bool missed_branch = false;
    bool had_else = false;
    bool success = true;
        
    do {
        Token *stmt;
        found_this_branch = false;
        // If it's not an ELSE, skip over the expression at the beginning.
        if (tree->id == ELSE) {
            stmt = tree->child;
            had_else = true;
        } else
            stmt = tree->child->next;
                
        if (!pass1_in_control_stmt(stmt, sub, found_this_branch)) {
            success = false;
        }
        
        // If we didn't find it this branch, mark that a branch was missed.
        missed_branch |= !found_this_branch;
        
        if (!tree->next)
            break;
        if (tree->next->id != ELSEIF && tree->next->id != ELSE)
            break;
        tree = tree->next;
    } while (true);
    
    // We only found an unconditional return if there were no missed branches
    // and there was an ELSE branch.
    foundRet = !missed_branch & had_else;
    
    return success;
}

bool Component::pass1_select(Token *&tree, Sub *sub, bool &foundRet) {
    bool found_this_branch;
    bool missed_branch = false;
    bool had_default = false;
    bool success = true;
        
    // If there are no case branches, then we obviously didn't find the return.
    if (!tree->next || (tree->next->id != CASE && tree->next->id != DEFAULT)) {
        foundRet = false;
        return true;
    }
    
    tree = tree->next;
    
    do {
        Token *stmt;
        found_this_branch = false;
        // Skip the expression for CASE blocks.
        if (tree->id == DEFAULT) {
            had_default = true;
            stmt = tree->child;
        } else {
            stmt = tree->child->next;
        }
        
        if (!pass1_in_control_stmt(stmt, sub, found_this_branch)) {
            success = false;
        }
        
        missed_branch |= !found_this_branch;
        
        if (!tree->next)
            break;
        if (tree->next->id != CASE && tree->next->id != DEFAULT)
            break;
        tree = tree->next;
    } while (true);
    
    foundRet = !missed_branch & had_default;
    
    return success;
}

bool Component::pass1_for(Token *tree, Sub *sub, bool &foundRet) {
    // Skip over the <init>, <final>, and <step> expressions
    // and return false if there is nothing there.
    Token *stmt = tree->child->next->next;
    if (!stmt)
        return false;
    if (stmt->id == STEP)
        stmt = stmt->next;
    if (!stmt)
        return false;
    
    return pass1_in_control_stmt(stmt, sub, foundRet);
}

// A quick note about this: technically an infinite loop with a single RETURN
// statement somewhere is defined to always return (otherwise it loops
// indefinitely). However, because of what this analysis is for, we are not
// going to look at that case. It would make it too difficult to find where
// the SUB actually ends by looking at the code.
bool Component::pass1_do_or_while(Token *tree, Sub *sub, bool &foundRet) {
    // Skip over the condition
    Token *stmt = tree->child->next;
    if (!stmt) {
        foundRet = false;
        return true;
    }
        
    return pass1_in_control_stmt(stmt, sub, foundRet);
}

bool Component::pass1_sub(Token *&tree) {
    bool found = false;
    bool success;
    
    int first_line = tree->loc.first_line;
    int current_line = first_line + tree->loc.num_lines;
    
    Token *head = tree;
    
    // If it was a label, add a ret-void node
    if (head->id == LABEL) {
        head->id = SUB;
        head->child->next = token_from_text("ret-void", AS);
    }
    
    // This also appends the nodes in head->next to the body of the sub.
    // Then it is our job here to find where they end, and remove them from
    // the top level tree.
    Sub *me;
    success = addSubOrDeclare(head, me);
    
    // If we couldn't add it, fail, but still organize the statements as
    // a sub to help with other error checking.
    
    while (tree->next) {
        tree = tree->next;
        current_line = tree->loc.first_line + tree->loc.num_lines;
        switch (tree->id) {
        case IF:
            success &= pass1_if(tree, me, found);
            break;
            
        case SELECT:
            success &= pass1_select(tree, me, found);
            break;
            
        case FOR:
            success &= pass1_for(tree, me, found);
            break;
            
        case WHILE:
        case DO:
            success &= pass1_do_or_while(tree, me, found);
            break;
            
        case SUB:
            printf("error: nested SUBs not allowed (line %d)\n",
                tree->loc.first_line);
            // Don't keep going, because who knows if this should have been
            // a new sub or something else.
            return false;
            break;
            
        case LABEL:
            success &= pass1_label(tree, me);
            break;
            
        case TYPE:
            success &= pass1_type(tree);
            break;
            
        case END:
        case RETURN:
            // unlink the nodes that used to follow the SUB node.
            // they are now children nodes.
            head->next = tree->next;
            tree->next = 0;
            tree = head;
            return success;
            break;
        }
        
        if (found) {
            // This means we found it, but it was inside of some control
            // statement, so it could be hard to tell where the sub ends.
            // Putting a return statement at the end anyways will make
            // this warning go away.
            if (tree->next && tree->next->id == RETURN) {
                Token *unused_ret = tree->next;
                tree = tree->next = tree->next->next;
                token_free(unused_ret);
            } else {
                printf("warning: expected RETURN (potentially unclear)"
                    " (line %d)\n", current_line);
            }
            
            // unlink the nodes from being siblings to the SUB
            // (they are now children)
            head->next = tree->next;
            tree->next = 0;
            tree = head;
            return success;
        }
    }
    
    printf("error: SUB doesn't return (line %d)\n", current_line);
    head->next = 0;
    tree = head;
    return false;
}

bool Component::pass1_declare(Token *tree) {
    Sub *throwaway;
    return addSubOrDeclare(tree, throwaway);
}

bool Component::pass1_label(Token *tree, Sub *sub) {
    if (!sub->addLabel(tree->child->text)) {
        printf("error: duplicate label (line %d)\n",
            tree->child->loc.first_line);
        return false;
    }
    
    return true;
}

bool Component::pass1_type(Token *tree) {
    int align = 0; // default alignment
    
    // (TYPE name <number>)? <= alignment specifier
    if (tree->child->next && tree->child->next->id == ICON) {
        align = atoi(tree->child->next->text);
        // Only allow 1, 2, 4, 8
        if (align != 1 && align != 2 && align != 4 && align != 8) {
            printf("warning: alignment must be 1, 2, 4, or 8;"
                " ignoring (line %d)\n",
                tree->child->next->loc.first_line);
            align = 0;
        }
    }
    
    std::string name(tree->child->text);
    Struct *st = new Struct(name, align);
    if (!insertType(name, st)) {
        printf("error: duplicate type (line %d)\n",
            tree->child->loc.first_line);
        delete st;
        return false;
    }
    
    return true;
}

bool Component::addSubOrDeclare(Token *tree, Sub *&me) {
    bool is_declare;
    bool type_only = false;
    bool isCdecl = false;
    int declflags;
    char *libp = 0;
    
    if (tree->id == DECLARE) {
        is_declare = true;
        declflags = 0;
    } else {
        is_declare = false;
        declflags = Sub::IMPLEMENTED;
    }
    
    // If it's a declare, we might have a string literal here
    Token *idnode = tree->child;
    if (idnode->id == SCON) {
        // scan the string for flags
        int i;
        for (i = 1; ; i++) {
            switch (idnode->text[i]) {
            case '+':
                declflags |= Sub::COMMAND;
                break;
            case '!':
                isCdecl = true;
                break;
            case '#':
                type_only = true;
                break;
            case '"':
                if (idnode->text[i+1] != 0)
                    declflags |= Sub::HAS_LIB;
                goto stop_scanning;
            default:
                declflags |= Sub::HAS_LIB;
                goto stop_scanning;
            }
        }
    stop_scanning:
        if (declflags & Sub::HAS_LIB)
            libp = &idnode->text[i];
        
        idnode = idnode->next;
    }
    
    assert(idnode && "sub/declare without a name!");
    
    Token *param = idnode->next;
    
    // Insert this sub into the list
    std::string name(idnode->text);
        
    SubType *type = 0;

    if (!type_only) {
        me = getSub(name);
        
        if (!me) {
            me = insertSub(name, declflags);

            type = new SubType("{" + name + "}");
            
            // Use the size_t constructor of string to chop off the ending '"'.
            if (libp)
                me->setLib(std::string(libp, strlen(libp)-1));
        } else {
            type = const_cast<SubType *>(
                static_cast<const SubType *>(me->getType())
            );
            if (is_declare) {
                printf("error: SUB '%s' declared twice (line %d)\n",
                    idnode->text, idnode->loc.first_line);
                
                return false;
            } else if (me->getFlags() & Sub::IMPLEMENTED) {
                printf("error: only one implementation of SUB '%s' allowed"
                    " (line %d)\n", idnode->text,
                    idnode->loc.first_line);
                
                return false;
            } else if (me->getFlags() & Sub::HAS_LIB) {
                printf("error: implementation not allowed for SUB '%s' with "
                    "external library (line %d)\n", idnode->text,
                    idnode->loc.first_line);
            } else {
                me->setImplFlag();
                
                // This will also check if the return types are equal
                if (!paramListsEqual(param, me)) {
                    printf("error: DECLARE/SUB with non-matching parameters "
                        "or return type for '%s' (line %d)\n",
                        idnode->text, idnode->loc.first_line);
                    return false;
                }
                
                assert(param->next == 0 &&
                    "paramListsEqual didn't move param to end");
            }
        }
    }
    
    if (param->id == ALIAS) {
        assert(me && "parser should have disallowed function ptr with alias");
        // If it's quoted take off the quotes.
        if (param->child->id == SCON)
            me->setAlias(std::string(param->child->text+1,
                strlen(param->child->text+1)-1));
        else
            me->setAlias(param->child->text);
        
        param = param->next;
    }
    
    while (param->next) {
        if (param->id == DEF) {
            const Type *paramty = getType(param->child);
            if (!paramty) {
                printf("error: undefined type %s (line %d)\n",
                    param->child->text,
                    param->child->loc.first_line);
                
                return false;
            }
            type->addParamType(paramty);
                
            std::string pname(param->child->next->text);
            if (!type_only) {
                if (!me->addParam(pname, new Param(pname, paramty))) {
                    printf("error: duplicate variable name '%s' (line %d)\n",
                        pname.c_str(), param->child->next->loc.first_line);
                    return false;
                }
            }
        } else if (param->id == ELLIPSIS) {
            if (param->child) {
                if (param->child->id == LEN) {
                    type->addFlags(
                        SubType::VARARGS | SubType::VA_IMPLICIT_LEN
                        );
                } else if (param->child->id == ICON) {
                    type->addFlags(
                        SubType::VARARGS | SubType::VA_IMPLICIT_NULL
                        );
                } else {
                    assert(0 && "ellipsis has an unknown child node");
                }
            } else {
                type->addFlags(SubType::VARARGS);
            }
        }
        param = param->next;
    }
    
    // Now param should point to the return type node.
    // If there is no child node, it returns void.
    assert(param->id == AS && "return type node must always be the last one");
    
    if (param->child) {
        const Type *rtype = getType(param->child);
        if (!rtype) {
            printf("error: undefined type %s (line %d)\n",
                param->child->text,
                param->child->loc.first_line);
            
            return false;
        }
        
        // If they're not equal, it's just because the previous one is void,
        // otherwise paramListsEqual would have caught it. A warning was
        // already issued for this.
        const Type *pre_rtype = type->getReturnType();
        if (!pre_rtype || *pre_rtype != *rtype)
            type->setReturnType(rtype);
    } else {
        if (!type->getReturnType())
            type->setReturnType(void_type);
    }
    
    // This will fail anyways if the type already exists.
    if (isCdecl) {
        type->addFlags(SubType::CDECL);
    }
    insertType(type->getName(), type);
    
    if (type_only) {
        // Create a pointer to function type without the {}'s.
        insertType(name, type->getPtrType());
    }
        
    if (!type_only)
        me->setType(type);
        
    // insert the nodes following the sub into the actual body.
    if (!is_declare)
        param->next = tree->next;
        
    return true;
}

// When this function returns, tree must point to the last node in the SUB
// header (the return type).
bool Component::paramListsEqual(Token *&tree, Sub *sub) const {
    
    unsigned int index = 0;
    Var *var = sub->getVar(0);
    
    // Can't use an early return because of pre-requirement (see above).
    bool iseq = true;
    
    while (tree->id == DEF) {
        // Check if the pre-existing list is shorter
        if (!var) {
            iseq = false;
            tree = tree->next;
            continue;
        }
        
        const Type *type = getType(tree->child);
        std::string newname;
        
        if (!type)
            iseq = false;
        else if (*type != *var->getType())
            iseq = false;
        else if (!strEqualIdLookup(var->getName(),
                newname = tree->child->next->text)) {
        
            printf("warning: param names differ from DECLARE to SUB, "
                "using '%s' instead of '%s' (line %d)\n",
                tree->child->next->text, var->getName().c_str(),
                tree->child->next->loc.first_line);
            
            // MUST NOT call var->setName, otherwise sub's var map won't
            // get updated.
            if (!sub->setVarName(var->getName(), newname)) {
                printf("Just kidding; there was a conflict with that "
                    "name. I'm still gonna try to compile this, but you "
                    "should really consider synchronizing the names.\n");
            }
        }
        
        var = sub->getVar(++index);
        tree = tree->next;
    }
    
    SubType *subty = const_cast<SubType *>(
        static_cast<const SubType *>(sub->getType())
    );
    
    // Does it have var-args?
    if (tree->id == ELLIPSIS) {
        if ((subty->getFlags() & SubType::VARARGS) == 0)
            iseq = false;
        else if (tree->child) {
            // Is it an implicit len or null?
            if (tree->child->id == LEN &&
                    ((subty->getFlags() & SubType::VA_IMPLICIT_LEN) == 0))
                iseq = false;
            else if (tree->child->id == ICON &&
                    ((subty->getFlags() & SubType::VA_IMPLICIT_NULL) == 0))
                iseq = false;
        }
            
        tree = tree->next;
    } else {
        if ((subty->getFlags() & SubType::VARARGS) != 0)
            iseq = false;
    }
    
    assert(tree->id == AS && "no return type for sub");
    
    // If any of the params were not equal, it doesn't matter what the return
    // type is, and we can just return false here. Also make sure the
    // pre-existing list is the same length.
    if (!iseq || var)
        return false;
    
    // Make sure the return types are equal also.
    const Type *other_rettype = subty->getReturnType();
    
    // If you left off the return type from the SUB, it is carried over from
    // the declare, but a warning is shown.
    if (tree->child == 0) {
        if (*other_rettype != *void_type) {
            printf("warning: omitted return type, using '%s' (line %d)\n",
                other_rettype->getName().c_str(), tree->loc.first_line);
        }
        return true;
    }
    
    const Type *rettype = getType(tree->child);
    return rettype && *rettype == *other_rettype;
}

Component::Component() {
    // Add all the intrinsic types
    // Manually insert the void type. Its name in the lookup table should not
    // be a valid identifier.
    void_type = new IntrinsicType(IntrinsicType::VOID);
    types["#void#"] = void_type;
    
    insertType("bool", new IntrinsicType(IntrinsicType::BOOL));
    
    Type *char_type = new IntrinsicType(IntrinsicType::CHAR);
    types["char"] = char_type;
    types["char*"] = char_type->getPtrType();
    
    insertType("schar", new IntrinsicType(IntrinsicType::SCHAR));
    insertType("word", new IntrinsicType(IntrinsicType::WORD));
    insertType("sword", new IntrinsicType(IntrinsicType::SWORD));
    insertType("uint", new IntrinsicType(IntrinsicType::UINT));
    insertType("int", new IntrinsicType(IntrinsicType::INT));
    insertType("uint64", new IntrinsicType(IntrinsicType::UINT64));
    insertType("int64", new IntrinsicType(IntrinsicType::INT64));
    insertType("float", new IntrinsicType(IntrinsicType::FLOAT));
    insertType("double", new IntrinsicType(IntrinsicType::DOUBLE));
    
    const Type *ptr_type = void_type->getPtrType();
    types["pointer"] = ptr_type;
    types["#void#*"] = ptr_type;

    void_sub_type = new SubType("{}");
    const_cast<SubType *>(void_sub_type)->setReturnType(void_type);
    types["{}"] = void_sub_type;
    
    SubType *main_type = new SubType("{_GB_main}");
    main_type->setReturnType(void_type);
    main_type->addFlags(SubType::CDECL);
    types["{_gb_main}"] = main_type;
    implicit_main = new Sub(
        "_GB_main",
        main_type,
        Sub::IMPLEMENTED
    );
    vars["_gb_main"] = implicit_main;
}

Component::~Component() {
    // If there are any duplicate names, we end up deleting stuff twice
    // which is bad. Either don't allow duplicates or figure out a way
    // around this...
    types.erase("pointer");
    for (auto it = types.begin(); it != types.end(); ++it) {
        delete it->second;
    }
    for (auto it = vars.begin(); it != vars.end(); ++it) {
        delete it->second;
    }
}

Sub *Component::getSub(const std::string &name) const {
    var_map::const_iterator entry = vars.find(getIdLookupString(name));
        
    if (entry == vars.end())
        return 0;
    if (entry->second->exprClass() != Expression::SUB)
        return 0;
    return static_cast<Sub *>(entry->second);
}

Sub *Component::insertSub(const std::string &name, int flags) {
    std::string lower = getIdLookupString(name);
    if (vars.find(lower) != vars.end())
        return 0;
    Sub *sub = new Sub(name, void_sub_type, flags);
    vars[lower] = sub;
    return sub;
}

const Type *Component::getType(const std::string &name) const {
    type_map::const_iterator entry =
        types.find(getIdLookupString(name));
    
    if (entry == types.end())
        return 0;
    return entry->second;
}

bool Component::insertType(const std::string &name, const Type *type) {
    std::string lower = getIdLookupString(name);
    if (types.find(lower) != types.end())
        return false;
    types[lower] = type;
    return true;
}

Var *Component::getVar(const std::string &name) const {
    var_map::const_iterator entry =
        vars.find(getIdLookupString(name));
        
    if (entry == vars.end())
        return 0;
    
    return entry->second;
}

bool Component::insertVar(const std::string &name, Var *var) {
    std::string lower = getIdLookupString(name);
    if (vars.find(lower) != vars.end())
        return false;
    vars[lower] = var;
    return true;
}

// We have to make two passes through the tree. The first is the auto-declare
// feature that adds all subs and declares to the table. 
Component *Component::fromTree(Token *tree, bool del) {
    Component *p = new Component();

    if (!p->pass1(tree)) {
        printf("glazc: failed pass1\n");
        delete p;
        return 0;
    }
    
    if (!p->pass2(tree)) {
        printf("glazc: failed pass2\n");
        delete p;
        return 0;
    }
    
    if (del)
        token_free_all(tree);
    
    return p;
}

