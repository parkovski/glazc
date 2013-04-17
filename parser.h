#ifndef PARSER_H
#define PARSER_H

#include "scanner.h"

namespace glaz {

class Diagnostics;

class Parser {
    Scanner *scanner;
    Diagnostics *diag;
    
    const std::string filename;
    
    int current;
    SourceLocation current_loc;
    
    int lookahead;
    SourceLocation lookahead_loc;
    
    Token *old;
    // If old was referenced (returned by gettok), then we shouldn't delete it.
    bool old_referenced;
    
    bool had_error;
    
    Parser(Scanner *sc, const std::string &filename, Diagnostics *diag) :
        scanner(sc),
        diag(diag),
        filename(filename),
        old(0),
        old_referenced(false),
        had_error(false) { }
            
    int scan() {
        if (old) {
            if (!old_referenced)
                token_free(old);
            old = 0;
            old_referenced = false;
            current_loc = lookahead_loc;
            return current = lookahead;
        }
        return current = glaz::scan(scanner, &current_loc);
    }
    
    int getLookahead() {
        if (old) {
            // Get rid of the old token and move lookahead to current
            scan();
            // And get the lookahead token now that old is null.
            return getLookahead();
        }
        
        old = token_from_scanner(scanner, current, &current_loc);
        return lookahead = glaz::scan(scanner, &lookahead_loc);
    }
    
    Token *gettok() {
        if (old) {
            old_referenced = true;
            return old;
        }
        return token_from_scanner(scanner, current, &current_loc);
    }
    
    std::string toktext() {
        if (old) {
            return std::string(old->text);
        }
        return std::string((const char *)scanner->tok,
            (size_t)(scanner->cur - scanner->tok));
    }
    
    void error(const char *text);
    void warn(const char *text);
    
    Token *statement(Token *&tail);
    Token *restrictedStmt();
    void skipToStmtSep();
    int stmtSeparator(bool skipJunk = false);
    bool skipTo(int tok, bool acceptToken = true);
    
    Token *lvalue();
    Token *dotlessLvalue();
    Token *call(Token *);
    Token *assign(Token *);
    // possibly replace with operator precedence parser?
    Token *primary();
    
    int getPrec() const;
    Token *operPrecExpr(Token *lhs, int prec);
    Token *optExpr();
    Token *expr();
    Token *optExprList(Token *&tail);
    Token *exprList(Token *&tail);
    Token *optArrayAccess();
    Token *typeName();
    
    Token *subParamList(Token *&tail);
    
    // top level statements
    Token *printStmt();
    Token *gotoGosubStmt();
    Token *labelStmt();
    Token *declareStmt();
    Token *subStmt();
    Token *returnStmt();
    Token *defStmt();
    Token *constStmt();
    Token *setidStmt();
    Token *typeStmt();
    Token *autodefineStmt();
    Token *ifStmt(Token *&tail);
    Token *selectStmt(Token *&tail);
    Token *forStmt();
    Token *whileStmt();
    Token *doStmt();
    
public:
    ~Parser();
    
    static Parser *create(const char *filename);
    
    // Returns a Token representing the first statement in the program.
    // child -> the rest of this statement
    // next -> the head of the next statement
    Token *parse();
    bool failed() const { return had_error; }
};

}

#endif        //  #ifndef PARSER_H

