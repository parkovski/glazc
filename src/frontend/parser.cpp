#include "glaz.h"
#include <iostream>
#include "parser.h"
#include "diag.h"

using namespace glaz;

Parser::~Parser() {
    delete diag;
    scanner_deinit(scanner);
}

// Used in place of a constructor, because it can return NULL if the file
// couldn't be opened.
Parser *Parser::create(const char *filename) {
    Scanner *s = scanner_init(filename);
    if (!s) return nullptr;
    // TODO: the commands functionality won't work with multiple parsers
    commands_put("true", INTRINSICID);
    commands_put("false", INTRINSICID);
    commands_put("null", INTRINSICID);
    return new Parser(s, filename, new Diagnostics());
}

void Parser::error(const std::string &message) {
    diag->error(filename, message, toktext(), current_loc);
}

void Parser::warning(const std::string &message) {
    diag->warning(filename, message, toktext(), current_loc);
}

// Decides what type of statement this is and calls the appropriate parser.
// The tail parameter is because some statements (if and select) actually
// return a list of statements for each of their nodes.
Token *Parser::statement(Token *&tail) {
    Token *stmt = nullptr;
    Token *settail = nullptr;
    
    switch (current) {
    case PRINT:
        stmt = printStmt();
        break;
    
    case GOTO:
    case JUMP:
    case CALL:
    case GOSUB:
        stmt = gotoGosubStmt();
        break;
        
    case LABEL:
        stmt = labelStmt();
        break;
    
    case DECLARE:
        stmt = declareStmt();
        break;
        
    case SUB:
        stmt = subStmt();
        break;
        
    case RETURN:
        stmt = returnStmt();
        break;
        
    case DEF:
    case DIM:
        stmt = defStmt();
        break;
        
    case CONST:
        stmt = constStmt();
        break;
        
    case SETID:
        stmt = setidStmt();
        break;
        
    case END:
        stmt = gettok();
        scan();
        break;
        
    case TYPE:
        stmt = typeStmt();
        break;
        
    case AUTODEFINE:
        stmt = autodefineStmt();
        break;
        
    case ID:
        if (getLookahead() == ':') {
            stmt = labelStmt();
            break;
        } else if (lookahead == SETID) {
            if (!strEqualIdLookup(toktext(), "string")) {
                error("expected 'string setid' or at least something correct");
            } else {
                stmt = setidStmt();
            }
            
            break;
        }
        // Fall through.
    case '#':
    case '*': {
        Token *lval = lvalue();
        if (!lval)
            break;
        
        if (current == '=')
            stmt = assign(lval);
        else
            stmt = call(lval);
        
        if (!stmt)
            token_free(lval);
    }
        break;
        
    case SYSVAR: {
        Token *lval = lvalue();
        if (!lval)
            break;
        
        if (current == '=')
            stmt = assign(lval);
        else
            error("expected assignment statement");
        
        if (!stmt)
            token_free(lval);
    }
        break;
        
    case COMMAND: {
        Token *lval = gettok();
        scan();
        stmt = call(lval);
        
        if (!stmt)
            token_free(lval);
    }
        break;
    
    case LET: {
        int next = scan();
        if (next == ID || next == '#' || next == '*') {
            Token *lval = lvalue();
            if (!lval)
                break;
            
            if (current == '=')
                stmt = assign(lval);
            else
                error("expected assignment statement");
            
            if (!stmt)
                token_free(lval);
        } else {
            error("expected assignment statement");
        }
    }
        break;
        
    case IF:
        stmt = ifStmt(settail);
        break;
        
    case SELECT:
        stmt = selectStmt(settail);
        break;
        
    case FOR:
        stmt = forStmt();
        break;
        
    case WHILE:
        stmt = whileStmt();
        break;
        
    case DO:
        stmt = doStmt();
        break;
        
    case WAITUNTIL:
        // construct a "DO:WAIT():UNTIL cond"
        stmt = token_from_text("do", DO);
        stmt->child = token_from_text("until", UNTIL);
        scan();
        stmt->child->child = expr();
        stmt->child->next = token_from_text("call", CALL);
        stmt->child->next->child = token_from_text("wait", ID);
        break;
        
    default:
        error("expected statement");
        break;
    }
    
    // If there were errors, skip the rest of the line.
    if (stmt == nullptr)
        skipToStmtSep();
    else {
        if (settail)
            tail = settail;
        else
            tail = stmt;
    }
    
    return stmt;
}

// Called from single line IF statements. Mostly prevents other multiple line
// statements from being accepted.
Token *Parser::restrictedStmt() {
    Token *stmt = nullptr;
    
    switch (current) {
    case PRINT:
        stmt = printStmt();
        break;
    
    case GOTO:
    case JUMP:
    case CALL:
    case GOSUB:
        stmt = gotoGosubStmt();
        break;
            
    case RETURN:
        stmt = returnStmt();
        break;
        
    case END:
        stmt = gettok();
        scan();
        break;
        
    case ID:
    case '#':
    case '*': {
        Token *lval = lvalue();
        if (!lval)
            break;
        
        // Note: the restricted version of a call requires parenthesis.
        // However, calls to COMMANDs still can leave them off.
        
        if (current == '=')
            stmt = assign(lval);
        else if (current == '(')
            stmt = call(lval);
        else
            error("expected assignment or call");
        
        if (!stmt)
            token_free(lval);
    }
        break;
        
    case COMMAND: {
        Token *lval = gettok();
        scan();
        stmt = call(lval);
        
        if (!stmt)
            token_free(lval);
    }
        break;
    
    case LET: {
        int next = scan();
        if (next == ID || next == '#' || next == '*') {
            Token *lval = lvalue();
            if (!lval)
                break;
            
            if (current == '=')
                stmt = assign(lval);
            else
                error("expected assignment statement");
            
            if (!stmt)
                token_free(lval);
        } else {
            error("expected assignment statement");
        }
    }
        break;
        
    default:
        error("expected statement");
        break;
    }
    
    // If there were errors, skip the rest of the line.
    if (stmt == nullptr)
        skipToStmtSep();
    
    return stmt;
}

void Parser::skipToStmtSep() {
    while (current != EOL && current != ':' && current != EOI)
        scan();
}

// Returns the type of the last separator present, so that end-of-input
// can be easily tested for. This will always return a valid statement
// separator value, even if it skips to the end of the program.
// The skipJunk parameter mimics the behavior of IBasic & similar by ignoring
// junk at the end of the line without an error.
int Parser::stmtSeparator(bool skipJunk) {
    if (current != EOL && current != ':' && current != EOI) {
        if (!skipJunk)
            error("expected end of line");
        
        // Skip until next statement separator for error recovery
        skipToStmtSep();
        
        // Fall through and skip over any separators.
    }
    
    int old = current;
    while (current == EOL || current == ':') {
        old = current;
        scan();
    }
    
    if (current == EOI)
        return EOI;
    
    return old;
}

// Skips until tok, or a statement separator. Returns true if it found tok.
// If acceptColon is false only EOL/EOI are accepted as separators.
// This is useful inside of DEF statements.
bool Parser::skipTo(int tok, bool acceptColon) {
    while (current != tok) {
        if (current == EOL || current == EOI || (current == ':' && acceptColon))
            return false;
        scan();
    }
    
    return true;
}

// Reads an lvalue.
// lvalue ::= dotless-lvalue array* ('.' dotless-lvalue array*)*
//          | '*' '(' expr ')' array*
//          | '*' lvalue array*
//          | SYSVAR
Token *Parser::lvalue() {
    if (current == '*') {
        Token *token = gettok();
        token->id = DEREF;
        if (scan() == '(') {
            scan();
            token->child = expr();
            
            if (!token->child) {
                token_free(token);
                return nullptr;
            }
            
            if (current != ')') {
                error("expected ')'");
                token_free(token);
                return nullptr;
            }
            scan();
        } else {
            token->child = lvalue();
            
            if (!token->child) {
                error("expected something that makes sense");
                token_free(token);
                return nullptr;
            }
        }
        
        Token *current = token->child;
        while ((current = current->next = optArrayAccess()))
            ;
        return token;
    } else if (current == SYSVAR) {
        Token *token = gettok();
        scan();
        return token;
    }
    
    Token *head = dotlessLvalue();
    Token *node = head;
    Token *array = nullptr;
    
    if (!head) {
        return nullptr;
    }
    
    if ((array = optArrayAccess())) {
        node->child = array;
        while ((array->next = optArrayAccess()))
            array = array->next;
    }
    
    if (current == '.') {
        Token *dotless = head;
        head = gettok();
        head->child = dotless;
        scan();
        node = node->next = dotlessLvalue();
        if (!node) {
            error("expected lvalue (identifier)");
            token_free_all(head);
            return nullptr;
        }
        
        if ((array = optArrayAccess())) {
            node->child = array;
            while ((array->next = optArrayAccess()))
                array = array->next;
        }
    }
    
    while (current == '.') {
        scan();
        node = node->next = dotlessLvalue();
        if (!node) {
            error("expected lvalue (identifier)");
            token_free_all(head);
            return nullptr;
        }
        
        if ((array = optArrayAccess())) {
            node->child = array;
            while ((array->next = optArrayAccess()))
                array = array->next;
        }
    }
    
    return head;
}

// Reads a dotless-lvalue.
// dotless-lvalue ::= ID
//                  | '#' ID
Token *Parser::dotlessLvalue() {
    Token *token = gettok();
    
    if (current == ID) {
        scan();
    } else if (current == '#') {
        scan();
        if (current != ID) {
            error("expected identifier");
            token_free(token);
            return nullptr;
        }
        token->child = gettok();
        scan();
    } else {
        token_free(token);
        return nullptr;
    }
    
    return token;
}

// Reads a function call expression (no GOSUB). The lvalue will already have
// been parsed, and if the call is successful this should be the first
// child in the node.
Token *Parser::call(Token *lval) {
    bool isparen = current == '(';
    
    if (isparen)
        scan();
    
    Token *first, *last;
    first = optExprList(last);
    
    if (isparen) {
        if (current != ')') {
            error("expected ')'");
            if (skipTo(')', /*acceptColon=*/false))
                scan();
            if (first)
                token_free_all(first);
            return nullptr;
        } else {
            scan();
        }
    }
    
    Token *head = token_from_text("call", CALL);
    head->child = lval;
    head->child->next = first;
    return head;
}

// Reads an assign statement. Similar to CALL, the lvalue has already been
// parsed. On the right side is an expression.
Token *Parser::assign(Token *lval) {
    Token *eq = gettok();
    eq->id = LET; // to not confuse with testing for equality.
    scan(); // '='
    
    Token *rhs = expr();
    if (!rhs) {
        token_free(eq);
        return nullptr;
    }
    
    lval->next = rhs;
    eq->child = lval;
    return eq;
}

// Reads a primary expression. This is one of:
// INTCONST (integer constant)
// DOUBLECONST (floating point constant)
// STRINGCONST (string constant)
// SYSVAR (i.e. constants created by SETID)
// lvalue (ID, member/array access, pointer dereference/cast)
// '&' lvalue (take address of something)
// call (COMMAND tokens don't require parenthesis)
// cast (to <type>(expr))
Token *Parser::primary() {
    Token *token;
    Token *callnode;
    
    switch (current) {
    case STRINGCONST:
    case INTCONST:
    case UINTCONST:
    case LONGINTCONST:
    case ULONGINTCONST:
    case FLOATCONST:
    case DOUBLECONST:
    case SYSVAR:
    case INTRINSICID:
        token = gettok();
        scan();
        return token;
        
    case '#':
    case '*':
    case ID:
        token = lvalue();
        if (!token)
            return nullptr;
        if (current == '(') {
            callnode = call(token);
            if (callnode)
                token = callnode;
            else {
                token_free_all(token);
                token = nullptr;
            }
        }
        return token;

    case COMMAND:
        token = gettok();
        scan();
        if (current == '(') {
            callnode = call(token);
        } else {
            // No parens = no params allowed
            callnode = token_from_text("call", CALL);
            callnode->child = token;
        }
        if (callnode)
            return callnode;
        else {
            token_free(token);
            return nullptr;
        }
        
    case '&':
        token = gettok();
        token->id = ADDROF;
        scan();
        token->child = lvalue();
        if (!token->child) {
            token_free(token);
            return nullptr;
        }
        return token;
        
    case '-':
        token = gettok();
        token->id = UMINUS;
        scan();
        token->child = primary();
        if (!token->child) {
            error("expected expression");
            token_free(token);
            return nullptr;
        }
        return token;
        
    case '(':
        scan();
        token = expr();
        if (!token) {
            // error recovery
            if (skipTo(')'))
                scan();
            return nullptr;
        }
        if (current != ')') {
            error("expected ')'");
            token_free(token);
            return nullptr;
        }
        scan(); // ')'
        return token;
        
    case TO:
        token = gettok();
        scan();
        if (!(token->child = typeName())) {
            token_free(token);
            return nullptr;
        }
                
        if (current != '(') {
            error("expected '('");
            token_free(token);
            return nullptr;
        }
        scan();
        
        if ((token->child->next = expr()) == nullptr) {
            token_free(token);
            return nullptr;
        }
        
        if (current != ')') {
            error("expected ')'");
            token_free(token);
            return nullptr;
        }
        scan();
        return token;
        
    default:
        return nullptr;
    }
}

int Parser::getPrec() const {
    switch (current) {
    case '&':
    case '|':
        return 1;
    
    case '<':
    case '>':
    case '=':
    case LEQ:
    case GEQ:
    case NEQ:
        return 2;
        
    case RSHIFT:
    case LSHIFT:
        return 3;
    
    case '+':
    case '-':
        return 4;
        
    case '*':
    case '/':
    case '%':
    case XOR:
        return 5;
    
    case '^':
        return 6;
        
    default:
        return 0;
    }
}

// Operator precedence parser for expressions.
Token *Parser::operPrecExpr(Token *lhs, int prec) {
    Token *head = lhs, *right;
    
    int oldprec;
    
    while ((oldprec = getPrec()) >= prec) {
        head = gettok();
        scan();
        
        right = primary();
        
        if (!right) {
            error("expected expression");
            token_free(head);
            return nullptr;
        }
        
        int newprec;
        while ((newprec = getPrec()) > oldprec ||
                current == '^') {
                    
            right = operPrecExpr(right, newprec);
            oldprec = newprec;
        }
        
        head->child = lhs;
        head->child->next = right;
        lhs = head;
    }
    
    return head;
}

Token *Parser::optExpr() {
    Token *lhs = primary();
    if (!lhs)
        return nullptr;
        
    return operPrecExpr(lhs, 1);
}

Token *Parser::expr() {
    Token *e = optExpr();
    if (!e)
        error("expected expression");
    return e;
}

Token *Parser::optExprList(Token *&tail) {
    Token *head = optExpr();
    Token *expr = head;
    Token *next;
    
    while (expr) {
        if (current != ',')
            break;
        scan();
        
        next = this->expr();
        if (!next) {
            token_free_all(head);
            return nullptr;
        }
        
        expr = expr->next = next;
    }
    
    tail = expr;
    return head;
}

// This is the same as optExprList but logs an error when none was found.
Token *Parser::exprList(Token *&tail) {
    Token *list = optExprList(tail);
    if (!list)
        error("expected expression list");
    
    return list;
}

// If there is an '[', reads a list of expressions for array access.
Token *Parser::optArrayAccess() {
    if (current != '[')
        return nullptr;
        
    Token *list = gettok();
    scan();
    
    // It is optional, because there are some cases where you can define
    // arrays without explicit bounds.
    Token *tail;
    list->child = optExprList(tail);
    
    if (current != ']') {
        error("expected ']'");
    }
    scan();
    
    return list;
}

Token *Parser::typeName() {
    Token *head;
    if (current != ID && current != COMMAND) {
        error("expected type name");
        return nullptr;
    }
    
    head = gettok();
    head->id = ID;
    scan();
    
    // Convert int** to (* (* (int)))
    while (current == '*') {
        Token *star = gettok();
        scan();
        star->child = head;
        head = star;
    }
    
    return head;
}

// Parses parameters in a SUB/DECLARE statement.
// Param = ID {'[' bound {',' bound}*} ':'|AS ID {BYVAL|BYREF}
// Last param may be "..." => var-args, like C.
// It may also be either "LEN, ..." or "..., null" which cause either the number
// of arguments to be passed before the list, or a null to be passed as
// a terminator.
Token *Parser::subParamList(Token *&tail) {
    Token *head = nullptr;
    Token *tailtmp = nullptr;
    
    while (true) {
        if (current == LEN) {
            Token *lentok = gettok();
            if (scan() != ',' || scan() != ELLIPSIS) {
                error("expected ', ...' for length-prefixed var-args");
                token_free(lentok);
                if (head)
                    token_free_all(head);
                return nullptr;
            }
            
            // Current token is the '...'
            if (!head)
                head = tailtmp = gettok();
            else
                tailtmp = tailtmp->next = gettok();
            
            tailtmp->child = lentok;
            
            if (scan() == ',') {
                error("var-args (\"...\") must be the last argument");
                token_free_all(head);
                return nullptr;
            }
            
            break;
        } else if (current == ELLIPSIS) {
            if (!head)
                head = tailtmp = gettok();
            else
                tailtmp = tailtmp->next = gettok();
            
            if (scan() == ',') {
                // Is it ", null" for a NULL terminated list?
                if (getLookahead() == INTRINSICID) {
                    scan();
                    // Only allow the intrinsic "null".
                    if (std::string(scanner->tok, scanner->cur) == "null") {
                        tailtmp->child = gettok();
                        scan();
                        break;
                    }
                    
                    // Otherwise, report an error.
                    error("expected 'null' for null-terminated var-args");
                    if (head)
                        token_free_all(head);
                    return nullptr;
                }
                
                error("var-args (\"...\") must be the last argument");
                token_free_all(head);
                return nullptr;
            }
            break;
        }
        
        if (current != ID) {
            error("expected argument specifier");
            // If there are no parenthesis, it will skip until the next EOL.
            // This ensures we don't get multiple errors for the same thing.
            skipTo(')');
            if (head)
                token_free_all(head);
            return nullptr;
        }
        
        Token *local_head = token_from_text("def", DEF);
        if (head == nullptr)
            tailtmp = head = local_head;
        else
            tailtmp = tailtmp->next = local_head;
        
        // the format of these trees is DEF <type> <id>
        Token *id = gettok();
        if (scan() != ':' && current != AS)
            error("expected ':' or AS");
        else
            scan();
        
        if (!(tailtmp->child = typeName())) {
            token_free(id);
            token_free_all(head);
            return nullptr;
        }
        
        tailtmp->child->next = id;
        
        if (current == ',')
            scan();
        else
            break;
    }
    
    tail = tailtmp;
    return head;
}

// Only called when current == PRINT. The rest is a list of tokens.
// A PRINT statement is parsed in general like a regular function call
// with no arguments, except that it can have a trailing comma.
// If there is no trailing comma, a PRINTLN statement is constructed.
// Otherwise it is a PRINT statement (comma = no newline on the end).
Token *Parser::printStmt() {
    Token *head = gettok();
    head->id = PRINTLN;
    scan();
    
    // Use a custom expression list parser because of the trailing comma
    Token *expr_list = optExpr();
    if (!expr_list)
        return head;
    
    head->child = expr_list;
    Token *next;
    while (current == ',') {
        scan();
        next = optExpr();
        if (!next) {
            // No expression? Then don't print a new line.
            head->id = PRINT;
            break;
        }
        expr_list = expr_list->next = next;
    }
    
    return head;
}

// Parses GOTO, GOSUB, JUMP, and CALL, but not function calls with parameters.
Token *Parser::gotoGosubStmt() {
    Token *head = gettok();
    switch (current) {
    case GOTO:
    case JUMP:
        head->id = GOTO;
        break;
    
    case CALL:
    case GOSUB:
        head->id = CALL;
        break;
    }
    
    if (scan() != ID) {
        error("expected identifier");
        token_free(head);
        return nullptr;
    } else {
        head->child = gettok();
        scan();
    }
    
    return head;
}

// Parses a declaration of the type LABEL ID or ID ':'.
Token *Parser::labelStmt() {
    Token *head;
    if (current == LABEL) {
        head = gettok();
        
        if (scan() != ID) {
            error("expected identifier");
            // error recovery: fall through & pretend it was an ID
        }
        
        head->child = gettok();
        head->child->id = ID;
        scan();
    } else {
        // It's an ID
        head = token_from_text("label", LABEL);
        head->child = gettok();
        
        // Skip past the ID
        scan();
        // We can assume the next token is a colon, since this function
        // is only called for LABEL *** or ID ':'.
        scan();
    }
    
    return head;
}

// Parses a DECLARE statement. These can be preceded by a string, which
// can modify the meaning of the declare. This feature is extended from
// the original IBasic which supported declare "!somelib" for CDECL.
// Supported options are:
// - "!", CDECL, inherited from IBasic.
// - "+", which makes that function a command.
// - "#", which creates a function pointer type, not an actual function.
Token *Parser::declareStmt() {
    Token *head = gettok();
    Token *token = head;
    bool command = false, fnptr = false;
    
    // String?
    if (scan() == STRINGCONST) {
        Token *str = gettok();
        
        bool isCdecl = false;
        int index = 1; // skip past the first quote
        // Get the different options
        while (true) {
            switch (str->text[index]) {
            case '!':
                if (isCdecl)
                    warning("duplicate option \"!\"");
                isCdecl = true;
                break;
                
            case '+':
                if (command)
                    warning("duplicate option \"+\"");
                
                if (fnptr) {
                    error("can't combine \"+\" and \"#\" options");
                    // error recovery
                    fnptr = false;
                }
                command = true;
                break;
                
            case '#':
                if (fnptr)
                    warning("duplicate option \"#\"");
                
                if (command) {
                    error("can't combine \"+\" and \"#\" options");
                    // error recovery
                    command = false;
                }
                fnptr = true;
                break;
                
            default:
                goto break_main_loop;
            }
            
            ++index;
        }
        break_main_loop:
        
        // The function pointer option can't also be a library import
        // (str->text[index] should be a quote so skip it).
        if (fnptr && str->text[index+1] != '\0')
            error("can't use library name with \"#\" option");
        
        // Ignore the information from the string for now. It will be rescanned
        // later when the AST is built.
        
        token->child = str;
        token = token->child;
        
        if (scan() != ',')
            error("expected ','");
        else
            scan();
    }
    
    if (current != ID && current != COMMAND) {
        error("expected identifier");
        // Kind of pointless to try to continue
        token_free_all(head);
        return nullptr;
    }
    
    // If there was no string we need to create a child node here
    if (token->id == DECLARE)
        token = token->child = gettok();
    else
        token = token->next = gettok();
    
    // If it was a command make it an ID
    token->id = ID;
    
    // If there was a '+', add it to the dictionary of commands
    if (command)
        commands_put(token->text, COMMAND);
    
    // ALIAS ID|STRINGCONST|COMMAND
    if (scan() == ALIAS) {
        if (fnptr) {
            error("ALIAS not allowed with '#' specifier");
            skipTo('(', /*acceptColon=*/true);
        } else {
            Token *aliasnode = gettok();
            if (scan() == ID || current == STRINGCONST || current == COMMAND) {
                aliasnode->child = gettok();
                token = token->next = aliasnode;
                scan();
            } else {
                error("expected identifier");
                token_free(aliasnode);
            }
        }
    }
    
    if (current == '(') {
        if (scan() != ')') {
            Token *params_end;
            Token *params = subParamList(params_end);
            if (params) {
                token->next = params;
                token = params_end;
            }
            if (current != ')') {
                error("expected ')'");
                skipTo(')', /*acceptColon=*/false);
            }
        }
        
        // Is there a return type?
        if (scan() == ',' || current == ':' || current == AS) {
            Token *retnode = gettok();
            retnode->id = AS;
            scan();
            if (!(retnode->child = typeName())) {
                token_free_all(head);
                return nullptr;
            }
            token = token->next = retnode;
        } else {
            // add a return void node
            token = token->next = token_from_text("ret-void", AS);
        }
    } else {
        // add a return void node
        token = token->next = token_from_text("ret-void", AS);
    }
    
    return head;
}

// Similar to a declare, but not nearly as complicated:
// SUB ID (params), ret-type
Token *Parser::subStmt() {
    Token *head, *token;
    head = gettok();
    token = head;
    
    // You can use a command value here, otherwise how would they be
    // implemented?
    if (scan() != ID && current != COMMAND) {
        error("expected identifier");
        token_free(head);
        return nullptr;
    }
    
    token = token->child = gettok();
    // If it was a command make it an ID
    token->id = ID;
    
    if (scan() == '(') {
        if (scan() != ')') {
            Token *params_end;
            Token *params = subParamList(params_end);
            if (params) {
                token->next = params;
                token = params_end;
            }
            if (current != ')') {
                error("expected ')'");
                skipTo(')', /*acceptColon=*/false);
            }
        }
        
        // Is there a return type?
        if (scan() == ',' || current == ':' || current == AS) {
            Token *retnode = gettok();
            retnode->id = AS;
            scan();
            if (!(retnode->child = typeName())) {
                token_free_all(head);
                return nullptr;
            }
            token = token->next = retnode;
        } else {
            // add a return void node
            token = token->next = token_from_text("ret-void", AS);
        }
    } else {
        // add a return void node
        token = token->next = token_from_text("ret-void", AS);
    }
    
    return head;
}

Token *Parser::returnStmt() {
    Token *head = gettok();
    scan();
    Token *e = optExpr();
    // If it's null child won't change anyway.
    head->child = e;
    return head;
}

Token *Parser::defStmt() {
    Token *head = gettok();
    head->id = DEF; // in case it was DIM
    Token *namechain, *name;
    
    if (scan() != ID) {
        error("expected identifier");
        token_free(head);
        return nullptr;
    }
    
    namechain = gettok();
    scan();
    name = namechain;
    name->child = optArrayAccess();
    
    while (current == ',') {
        if (scan() != ID) {
            error("expected identifier");
            token_free(head);
            token_free_all(namechain);
            return nullptr;
        }
        name = name->next = gettok();
        scan();
        name->child = optArrayAccess();
    }
    
    if (current != ':' && current != AS) {
        error("expected ':' or AS");
        token_free(head);
        token_free_all(namechain);
        return nullptr;
    }
    scan();
    
    if (!(head->child = typeName())) {
        token_free(head);
        token_free_all(namechain);
        return nullptr;
    }
    
    // DEF <type> <names>
    head->child->next = namechain;
    return head;
}

// Parses a statement of the form CONST <id> = <expr>
// Constants can be of any intrinsic type.
Token *Parser::constStmt() {
    Token *head = gettok();
    
    if (scan() != ID) {
        error("expected identifier");
        token_free(head);
        return nullptr;
    }
    
    head->child = gettok();
    
    if (scan() != '=') {
        error("expected '='");
        token_free(head);
        return nullptr;
    }
    scan();
    
    Token *ex = expr();
    if (!ex) {
        token_free(head);
        return nullptr;
    }
    
    head->child->next = ex;
    return head;
}

// Parses a SETID or STRING SETID statement. The difference is that a
// STRING SETID is replaced with its value when it is found inside of strings.
Token *Parser::setidStmt() {
    Token *head;
    // SETID or STRING SETID?
    if (current != SETID) {
        scan();
        head = token_from_text("string setid", STRINGSETID);
    } else {
        head = gettok();
    }
    
    if (scan() != STRINGCONST) {
        error("expected string constant");
        token_free(head);
        return nullptr;
    }
    
    Token *scon = gettok();
    head->child = scon;
    // make sure it is a valid identifier
    // note: an empty setid is valid, and referenced by "@".
    for (int i = 1; scon->text[i] != '"' || scon->text[i+1] == '"'; i++) {
        int ch = scon->text[i];
        if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
            (ch >= '0' && ch <= '9') || (ch == '_')) {
        
            continue;
        }
        
        error("SETID must have a valid identifier");
        token_free(head);
        return nullptr;
    }
    
    if (scan() != ',') {
        error("expected ','");
        token_free(head);
        return nullptr;
    }
    scan();
    
    Token *ex = expr();
    if (!ex) {
        token_free(head);
        return nullptr;
    }
    
    head->child->next = ex;
    return head;
}

// Parses a type (structure) definition. A type consists of just a list of
// DEF statements, which we can use defStmt to parse.
Token *Parser::typeStmt() {
    Token *head = gettok();
    Token *node = head;
    
    if (scan() != ID && current != COMMAND) {
        error("expected identifier");
        token_free(head);
        return nullptr;
    }
    
    node = node->child = gettok();
    node->id = ID; // in case it was a command
    
    if (scan() == ',') {
        if (scan() != INTCONST) {
            error("expected integer constant");
            token_free(head);
            return nullptr;
        }
        node = node->next = gettok();
        scan();
    }
    
    while (true) {
        stmtSeparator();
        if (current == ENDTYPE) {
            scan();
            return head;
        }
        
        if (current == DEF) {
            Token *def = defStmt();
            if (def)
                node = node->next = def;
            else
                skipToStmtSep();
        } else {
            error("expected variable definition");
            token_free_all(head);
            return nullptr;
        }
    }
    // No need to return here. The loop will return when ENDTYPE or
    // something invalid is found.
}

Token *Parser::autodefineStmt() {
    Token *head = gettok();
    
    if (scan() == STRINGCONST) {
        // check value of string
        if (current_loc.num_columns == 4 || current_loc.num_columns == 5) {
            // Start at 1 because tok[0] is the quote character.
            if (scanner->tok[1] == 'o' || scanner->tok[1] == 'O') {
                if ((scanner->tok[2] == 'n' || scanner->tok[2] == 'N') &&
                    scanner->tok[3] == '"') {
                    head->child = gettok();
                    scan();
                    return head;
                }
                
                // don't need to test for the end quote since it can't be
                // any longer than this.
                if ((scanner->tok[2] == 'f' || scanner->tok[2] == 'F') &&
                    (scanner->tok[3] == 'f' || scanner->tok[3] == 'F')) {
                        
                    head->child = gettok();
                    scan();
                    return head;
                }
            }
        }
    }
    
    error("expected string value \"on\" or \"off\"");
    token_free(head);
    return nullptr;
}

// Parses an IF statement, any number of ELSE IFs, and a possible ELSE.
// If in the single line form, a THEN is required, and no ELSE IFs are allowed.
// Single line IFs are allowed to chain statements, but they must be separated
// by ':', no new lines. Statements in single line IFs are restricted.
Token *Parser::ifStmt(Token *&tail) {
    Token *head = gettok(); // IF
    Token *local_tail = head;
    Token *internal_tail; // for the list of statements inside the IF block
    bool scanned_else = false;
    scan();
    
    Token *ex = expr();
    if (!ex) {
        // pretend there was an expression for error recovery
        ex = token_from_text("0", INTCONST);
    }
    internal_tail = head->child = ex;
    
    if (current == THEN) {
        // it's a single line IF
        scan();
        internal_tail = internal_tail->next = restrictedStmt();
        if (!internal_tail) {
            token_free(head);
            return nullptr;
        }
        while (current == ':') {
            scan();
            internal_tail = internal_tail->next = restrictedStmt();
            if (!internal_tail) {
                token_free(head);
                return nullptr;
            }
        }
        
        if (current == ELSE) {
            local_tail = head->next = gettok();
            scan();
            internal_tail = local_tail->child = restrictedStmt();
            if (!internal_tail) {
                token_free_all(head);
                return nullptr;
            }
            while (current == ':') {
                scan();
                internal_tail = internal_tail->next = restrictedStmt();
                if (!internal_tail) {
                    token_free_all(head);
                    return nullptr;
                }
            }
        }
        
        tail = local_tail;
        return head;
    }
    
    // If we got to this point, we have read IF <expr>, and there is no THEN.
    // Keep appending to internal_tail. Append ELSE IFs and ELSE to local_tail.
    if (stmtSeparator() == EOI) {
        error("expected ENDIF");
        return head;
    }
    
    while (true) {
        Token *old_internal_tail, *new_internal_tail;
        old_internal_tail = internal_tail;
        
        switch (current) {
        case ELSE:
        case ENDIF:
            goto elseif_else_loop;
        }
        
        internal_tail = internal_tail->next = statement(new_internal_tail);
        if (!internal_tail) {
            // error recovery
            internal_tail = old_internal_tail;
        } else {
            internal_tail = new_internal_tail;
        }
        
        if (stmtSeparator() == EOI) {
            error("expected ENDIF");
            return head;
        }
    }
    
elseif_else_loop:
    if (current == ENDIF) {
        scan();
        tail = local_tail;
        return head;
    }
    
    if (scanned_else) {
        error("only one ELSE block allowed (and it must be the last)");
        // but allow it for error recovery
    }
    
    // otherwise it must be ELSE. Is it ELSE IF?
    local_tail = local_tail->next = gettok();
    if (scan() == IF) {
        scan();
        local_tail->id = ELSEIF;
        internal_tail = local_tail->child = expr();
        if (!internal_tail) {
            // error recovery: we're pretty deep in at this point.
            // no use discarding the whole IF block. create an expression
            // that would never get executed anyways.
            internal_tail = local_tail->child = token_from_text("0", INTCONST);
        }
    } else {
        scanned_else = true;
        
        // Do this so that we will insert the first node of the else branch
        // into the right place below.
        internal_tail = nullptr;
    }
    
    if (stmtSeparator() == EOI) {
        error("expected ENDIF");
        return head;
    }
    
    while (true) {
        Token *old_internal_tail, *new_internal_tail;
        old_internal_tail = internal_tail;
        
        switch (current) {
        case ELSE:
        case ENDIF:
            goto elseif_else_loop;
        }
        
        // If it's an ELSE, add a child. Otherwise the expr is the first child.
        if (!internal_tail)
            internal_tail = local_tail->child = statement(new_internal_tail);
        else
            internal_tail = internal_tail->next = statement(new_internal_tail);
        
        if (!internal_tail) {
            // error recovery
            internal_tail = old_internal_tail;
        } else {
            internal_tail = new_internal_tail;
        }
        
        if (stmtSeparator() == EOI) {
            error("expected ENDIF");
            return head;
        }
    }
}

// Parses a SELECT/CASE/DEFAULT/ENDSELECT statement.
Token *Parser::selectStmt(Token *&tail) {
    Token *head = gettok();
    scan();
    // Like the IF parser, local_tail refers to the CASE/DEFAULT node, and
    // internal_tail refers to the statement inside of the block.
    Token *local_tail;
    Token *internal_tail;
    
    bool parsed_default = false;
    
    head->child = expr();
    if (!head->child) {
        // fake an expression. doesn't matter what because it won't get
        // compiled anyways. just so long as it's semantically valid.
        head->child = token_from_text("0", INTCONST);
    }
    
    local_tail = head;
    
    if (stmtSeparator() == EOI) {
        error("expected ENDSELECT");
        return head;
    }
    
next_block:
    if (current == CASE) {
        local_tail = local_tail->next = gettok();
        scan();
        local_tail->child = expr();
        if (!local_tail->child) {
            // fake it, just like above.
            local_tail->child = token_from_text("0", INTCONST);
        }
        internal_tail = local_tail->child;
    } else if (current == DEFAULT) {
        if (parsed_default) {
            // it doesn't have to be the last though, unlike ELSE.
            error("only one DEFAULT block allowed");
            // error recovery: allow it.
        }
        parsed_default = true;
        local_tail = local_tail->next = gettok();
        internal_tail = nullptr;
        scan();
    } else if (current == ENDSELECT) {
        scan();
        tail = local_tail;
        return head;
    } else {
        error("code must be inside a CASE or DEFAULT block");
        // in this case, we'd really mess up the parse tree anyways
        // so we might as well just delete everything and return.
        // it probably doesn't happen very often anyways.
        // this is only possible before a block, so no need for token_free_all.
        token_free(head);
        return nullptr;
    }
    
    if (stmtSeparator() == EOI) {
        error("expected ENDSELECT");
        return head;
    }
    
    while (true) {
        Token *old_internal_tail, *new_internal_tail;
        old_internal_tail = internal_tail;
        
        // First check if we should open a new block.
        if (current == CASE || current == DEFAULT || current == ENDSELECT)
            goto next_block;
        
        // If it's a DEFAULT block and this is the first statement,
        // insert into 'child' node, not 'next'.
        if (!internal_tail)
            internal_tail = local_tail->child = statement(new_internal_tail);
        else
            internal_tail = internal_tail->next = statement(new_internal_tail);
        
        if (!internal_tail) {
            // error recovery: pretend there was nothing there to begin with.
            internal_tail = old_internal_tail;
        }
        
        if (stmtSeparator() == EOI) {
            error("expected ENDSELECT");
            return head;
        }
    }
}

// Parses a statement of the type:
// FOR <lvalue> '=' <expr> TO <expr> {'#'|STEP expr}
// The NEXT at the end has an optional (and ignored) lvalue argument.
// Important: In IBasic, all of these expressions are only evaluated once.
Token *Parser::forStmt() {
    Token *head = gettok();
    Token *token;
    
    scan();
    token = head->child = lvalue();
    if (!head->child) {
        token_free(head);
        return nullptr;
    }
    
    if (current != '=') {
        error("expected assignment statement");
        token_free(head);
        return nullptr;
    }
    
    // Note, we don't construct an actual assignment node here, because we
    // know how FOR loops work in BASIC and how they're represented here.
    scan();
    token = token->next = expr();
    if (!token) {
        token_free(head);
        return nullptr;
    }
    
    if (current != TO) {
        error("expected TO");
        token_free(head);
        return nullptr;
    }
    
    scan();
    token = token->next = expr();
    if (!token) {
        token_free(head);
        return nullptr;
    }
    
    if (current == '#' || current == STEP) {
        token = token->next = gettok();
        token->id = STEP;
        scan();
        token->child = expr();
        if (!token->child) {
            token_free(head);
            return nullptr;
        }
    }
    
    if (stmtSeparator() == EOI) {
        error("expected NEXT");
        return head;
    }
    
    while (true) {
        Token *tail;
        if (current == NEXT) {
            // optional lvalue
            if (scan() != EOL && current != ':' && current != EOI) {
                token = lvalue();
                if (token)
                    token_free(token);
            }
            return head;
        }
        
        token->next = statement(tail);
        if (token->next)
            token = tail;
        
        if (stmtSeparator() == EOI) {
            error("expected NEXT");
            return head;
        }
    }
}

// Parses a WHILE block. The condition is evaluated at every iteration
// unlike with the FOR loop.
Token *Parser::whileStmt() {
    Token *head = gettok();
    Token *token;
    
    scan();
    token = head->child = expr();
    if (!token) {
        token_free(head);
        return nullptr;
    }
    
    if (stmtSeparator() == EOI) {
        error("expected ENDWHILE");
        return head;
    }
    
    while (true) {
        Token *tail;
        if (current == ENDWHILE) {
            scan();
            return head;
        }
        
        token->next = statement(tail);
        if (token->next)
            token = tail;
        
        if (stmtSeparator() == EOI) {
            error("expected ENDWHILE");
            return head;
        }
    }
}

// Parses a DO block.
Token *Parser::doStmt() {
    Token *head = gettok();
    Token *term;
    Token *token = head;
    
    scan();
    
    if (stmtSeparator() == EOI) {
        error("expected UNTIL");
        // insert a dummy node for the expression
        term = token_from_text("until", WHILE);
        term->child = token_from_text("true", INTRINSICID);
        head->child = term;
        return head;
    }
    
    while (true) {
        Token *tail;
        if (current == UNTIL) {
            term = gettok();
            scan();
            term->child = expr();
            // error recovery:
            if (!term->child)
                term->child = token_from_text("true", INTRINSICID);
            // now insert it as the first node.
            term->next = head->child;
            head->child = term;
            return head;
        }
        
        // if it's on the DO node we need to insert into child.
        if (token->id == DO) {
            if ((token->child = statement(tail)))
                token = tail;
        } else {
            if ((token->next = statement(tail)))
                token = tail;
        }
        
        if (stmtSeparator() == EOI) {
            error("expected UNTIL");
            // insert a dummy node for the expression
            term = token_from_text("until", UNTIL);
            term->child = token_from_text("true", INTRINSICID);
            term->next = head->child;
            head->child = term;
            return head;
        }
    }
}

// Parses a list of statements separated by stmtSeparator and returns a list
// of Tokens representing the program.
Token *Parser::parse() {
    Token *token = nullptr;
    Token *next = nullptr;
    Token *head = nullptr;
    
    // Get the first token
    scan();
    
    // The program could start with statement separators.
    if (current == EOL || current == ':')
        stmtSeparator();
    
    // It could be an empty program.
    if (current == EOI)
        return nullptr;
    
    while (!head) {
        if (diag->shouldExit()) {
            std::cout << "too many errors. exiting.\n";
            token_free_all(head);
            return nullptr;
        }

        head = statement(token);
    
        if (stmtSeparator() == EOI) {
            if (diag->hadError()) {
                if (head) token_free_all(head);
                return nullptr;
            }
            return head;
        }
    }
    
    while (true) {
        if (diag->shouldExit()) {
            std::cout << "too many errors. exiting.\n";
            token_free_all(head);
            return nullptr;
        }

        if ((token->next = statement(next))) {
            token = next;
        }        

        if (stmtSeparator() == EOI) {
            if (diag->hadError()) {
                token_free_all(head);
                return nullptr;
            }
            return head;
        }
    }
}

