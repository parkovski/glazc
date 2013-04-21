#ifndef SCANNER_H
#define SCANNER_H

#ifdef __cplusplus
namespace glaz {
#endif

enum {
    /* operators */
    ELLIPSIS = 257,
    RSHIFTEQ,
    LSHIFTEQ,
    ADDEQ,
    SUBEQ,
    MULEQ,
    DIVEQ,
    MODEQ,
    ANDEQ,
    POWEQ,
    OREQ,
    XOREQ,
    RSHIFT,
    LSHIFT,
    INCR,
    DECR,
    ARROW,
    XOR,
    LEQ,
    GEQ,
    NEQ,
    
    /* general tokens */
    ID,
    COMMAND,
    SYSVAR,
    ICON,
    UCON,
    LCON,
    ULCON,
    FCON,
    DCON,
    SCON,
    EOL,
    EOI,
    
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
    LET,
    WAITUNTIL,
    USECOMPONENT,
    INTERNAL,
    ENDINTERNAL,
    
    /* non-keyword node header values */
    PRINTLN,
    STRINGSETID,
    UMINUS,
    ELSEIF,
    ADDROF,
    DEREF
};

typedef struct Scanner {
    /*int             fd;*/
    FILE            *file;
    unsigned int    line;
    const char      *filename;
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
    char text[1]; // must at least have space for the null character
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

