#ifndef SCANNER_H
#define SCANNER_H

#ifdef __cplusplus
namespace glaz {
#endif

enum TokenId {
    /* operators */
    ELLIPSIS = 257,     // ... (var-args)
    RSHIFTEQ,           // >>= (right shift & assign)
    LSHIFTEQ,           // <<= (left shift & assign)
    ADDEQ,              // += (add & assign)
    SUBEQ,              // -= (subtract & assign)
    MULEQ,              // *= (multiply & assign)
    DIVEQ,              // /= (divide & assign)
    MODEQ,              // %= (modulo & assign)
    ANDEQ,              // &= (bitwise and & assign)
    POWEQ,              // ^= (raise to power & assign)
    OREQ,               // |= (bitwise or & assign)
    XOREQ,              // ||= (xor & assign)
    RSHIFT,             // >> (shift right)
    LSHIFT,             // << (shift left)
    INCR,               // ++ (increment)
    DECR,               // -- (decrement)
    ARROW,              // -> (do we even use this?)
    XOR,                // || (xor)
    LEQ,                // <= (less than or equal)
    GEQ,                // >= (greater than or equal)
    NEQ,                // <> (not equal)
    
    /* general tokens */
    ID,
    INTRINSICID,        // a built in ID that resolves to a constant like true/false/null
    COMMAND,            // a function with special treatment from the parser
    SYSVAR,             // an @foo variable
    INTCONST,
    UINTCONST,
    LONGINTCONST,
    ULONGINTCONST,
    FLOATCONST,
    DOUBLECONST,
    STRINGCONST,
    EOL,                // end of line
    EOI,                // end of input (not called EOF b/c C reserves that)
    
    /* keywords */
    LABEL,
    GOTO,
    JUMP,
    GOSUB,
    CALL,
    DEF,
    DIM,
    AS,
    CONST,
    SETID,
    AUTODEFINE,
    TYPE,
    ENDTYPE,
    LEN,
    DECLARE,
    SUB,
    ALIAS,
    RETURN,
    IF,
    THEN,
    ELSE,
    ENDIF,
    SELECT,
    CASE,
    DEFAULT,
    ENDSELECT,
    FOR,
    TO,
    STEP,
    NEXT,
    WHILE,
    ENDWHILE,
    DO,
    UNTIL,
    END,
    NEW,
    DELETE,
    PRINT,
    LET,                // the "let" keyword and also the assignment node id
    WAITUNTIL,
    USECOMPONENT,
    INTERNAL,
    ENDINTERNAL,
    
    /* non-keyword node header values */
    PRINTLN,            // print statement without trailing ','
    STRINGSETID,        // a setid (@foo) that does string interpolation
    UMINUS,             // unary minus (negation)
    ELSEIF,             // the "elseif" node for the "else if" compound keyword
    ADDROF,             // & (address of)
    DEREF               // * or # (pointer dereference)
};

typedef struct Scanner {
    FILE            *file;
    unsigned int    line;
    const char      *filename;
    // lol good luck
    unsigned char   *bot, *tok, *ptr, *cur,
                    *pos, *lim, *top, *eof;
} Scanner;

typedef struct SourceLocation {
    unsigned int first_line;
    unsigned int first_column;
    /* num_lines is 0 for a token that stays on one line */
    unsigned short num_lines;
    /* num_columns however, is equal to the length of the token (so > 0) */
    unsigned short num_columns;
} SourceLocation;

/* Token is a linked chain for easy parse tree construction. */
typedef struct Token {
    unsigned id;
    struct Token *next;
    struct Token *child;
    SourceLocation loc;
    char text[1]; // scanner functions allocate enough room here for the full text of the token
} Token;

#ifdef __cplusplus
extern "C" {
#endif

/* SourceLocation parameter can be null. */
extern unsigned scan(Scanner *, SourceLocation *);
extern Scanner *scanner_init(const char *);
extern void scanner_deinit(Scanner *);

/* to create a token from the current text in a Scanner */
extern Token *token_from_scanner(const Scanner *, unsigned,
    const SourceLocation *);
/* if the text is not contained in a scanner. There is no SourceLocation
 * parameter because if you're not using a scanner, you probably don't
 * have a location for it (yet).
 */
extern Token *token_from_text(const char *text, unsigned);
/* frees only children, not siblings, and returns next sibling */
extern Token *token_free(Token *);
/* frees children and siblings */
extern void token_free_all(Token *);

/* puts a command into the global dictionary */
extern void commands_put(const char *word, int id);
/* gets a command from the global dictionary (0 = not found) */
extern int commands_get(const char *word);
/* the same as above, but for strings that are not null-terminated */
extern int commands_get_from_range(const char *wordbegin, const char *wordend);

#ifdef __cplusplus
} /* extern "C" */
} /* namespace glaz */
#endif

#endif        /*  #ifndef SCANNER_H */

