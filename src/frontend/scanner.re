#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#include "scanner.h"

typedef unsigned int uint;
typedef unsigned char uchar;

#define BSIZE   4096

#define YYCTYPE     uchar
#define YYCURSOR    cursor
#define YYLIMIT     s->lim
#define YYMARKER    s->ptr
#define YYFILL(n)   {cursor = fill(s, cursor);}

#define RET(i)  {tokenid = i; goto retpoint;}

static
uchar *fill(Scanner *s, uchar *cursor){
    if(!s->eof) {
        uint cnt = s->tok - s->bot;
        if(cnt){
            memcpy(s->bot, s->tok, s->lim - s->tok);
            s->tok = s->bot;
            s->ptr -= cnt;
            cursor -= cnt;
            s->pos -= cnt;
            s->lim -= cnt;
        }
        if((s->top - s->lim) < BSIZE){
            uchar *buf = (uchar*) malloc(((s->lim - s->bot) + BSIZE)*sizeof(uchar));
            memcpy(buf, s->tok, s->lim - s->tok);
            s->tok = buf;
            s->ptr = &buf[s->ptr - s->bot];
            cursor = &buf[cursor - s->bot];
            s->pos = &buf[s->pos - s->bot];
            s->lim = &buf[s->lim - s->bot];
            s->top = &s->lim[BSIZE];
            free(s->bot);
            s->bot = buf;
        }
		if((cnt = fread(s->lim, 1, BSIZE, s->file)) != BSIZE){
        /*if((cnt = read(s->fd, (char*) s->lim, BSIZE)) != BSIZE){*/
            s->eof = &s->lim[cnt]; *(s->eof)++ = '\n';
        }
        s->lim += cnt;
    }
    return cursor;
}

unsigned scan(Scanner *s, SourceLocation *loc){
    uchar *cursor = s->cur;
    unsigned tokenid;
    unsigned int first_line, first_column;
    
std:
    first_line = s->line;
    s->tok = cursor;
    first_column = 1 + (unsigned int)(s->tok - s->pos);
/*!re2c
any = [\000-\377];
D   = [0-9];
L   = [a-zA-Z_];
H   = [a-fA-F0-9];
E   = [Ee] [+-]? D+;
FS  = [fF];
US  = [Uu];
LS  = [Qq];
ULS = [Uu][Qq];
IC  = (("0" [xX] | "&" [Hh]) H+) | (D+);
DC  = (D+ E) | (D* "." D+ E?) | (D+ "." D* E?);
*/

/*!re2c
    "/""*"          { goto comment; }
    [';]            { goto linecomment; }
    'rem'           { goto linecomment; }
    
    "_"[ \t\v\f]*([';][^\n]*)?"\n"
    {
        if (cursor == s->eof) RET(EOI);
        s->line++;
        s->pos = cursor;
        goto std;
    }
    
    'label'         { RET(LABEL); }
    'goto'          { RET(GOTO); }
    'jump'          { RET(JUMP); }
    'gosub'         { RET(GOSUB); }
    'call'          { RET(CALL); }
    
    'def'           { RET(DEF); }
    'dim'           { RET(DIM); }
    'as'            { RET(AS); }
    'const'         { RET(CONST); }
    'setid'         { RET(SETID); }
    'autodefine'    { RET(AUTODEFINE); }
    'type'          { RET(TYPE); }
    'endtype'       { RET(ENDTYPE); }
    'len'           { RET(LEN); }
    
    'declare'       { RET(DECLARE); }
    'sub'           { RET(SUB); }
    'alias'         { RET(ALIAS); }
    'return'        { RET(RETURN); }
    
    'if'            { RET(IF); }
    'then'          { RET(THEN); }
    'else'          { RET(ELSE); }
    'endif'         { RET(ENDIF); }
    
    'select'        { RET(SELECT); }
    'case'          { RET(CASE); }
    'default'       { RET(DEFAULT); }
    'endselect'     { RET(ENDSELECT); }
    
    'for'           { RET(FOR); }
    'to'            { RET(TO); }
    'step'          { RET(STEP); }
    'next'          { RET(NEXT); }
    
    'while'         { RET(WHILE); }
    'endwhile'      { RET(ENDWHILE); }
    
    'do'            { RET(DO); }
    'until'         { RET(UNTIL); }
    'waituntil'     { RET(WAITUNTIL); }
    
    'end'           { RET(END); }
    
    'new'           { RET(NEW); }
    'delete'        { RET(DELETE); }
    
    'print'         { RET(PRINT); }
    
    'let'           { RET(LET); }
    
    'usecomponent'  { RET(USECOMPONENT); }
	'__internal'    { RET(INTERNAL); }
	'__endinternal' { RET(ENDINTERNAL); }
    
    "_"
    {
        printf("%s:%d:%d: warning: identifier _ is invalid"
            " (it is used for line continuation)\n",
            s->filename, first_line, first_column);
        goto std;
    }
    
    L (L|D|"@")* "$"?
    {
		tokenid = commands_get_from_range(
            (const char *)s->tok,
            (const char *)cursor);
        if (tokenid) {
            goto retpoint;
        } else {
            RET(ID)
        }
    }
    
    "@" (L|D)*      { RET(SYSVAR); }
    
    IC              { RET(INTCONST); }
    IC US           { RET(UINTCONST); }
    IC LS           { RET(LONGINTCONST); }
    IC ULS          { RET(ULONGINTCONST); }
    
    DC              { RET(DOUBLECONST); }
    DC FS           { RET(FLOATCONST); }
    
    (["] ("\"\""|any\[\n\"])* ["])
                    { RET(STRINGCONST); }
    
    "..."           { RET(ELLIPSIS); }
    ">>="           { RET(RSHIFTEQ); }
    "<<="           { RET(LSHIFTEQ); }
    "+="            { RET(ADDEQ); }
    "-="            { RET(SUBEQ); }
    "*="            { RET(MULEQ); }
    "/="            { RET(DIVEQ); }
    "%="            { RET(MODEQ); }
    "&="            { RET(ANDEQ); }
    "^="            { RET(POWEQ); }
    "|="            { RET(OREQ); }
    "||="           { RET(XOREQ); }
    ">>"            { RET(RSHIFT); }
    "<<"            { RET(LSHIFT); }
    "++"            { RET(INCR); }
    "--"            { RET(DECR); }
    "->"            { RET(ARROW); }
    "||"            { RET(XOR); }
    "<="            { RET(LEQ); }
    ">="            { RET(GEQ); }
    "<>"            { RET(NEQ); }
    "~"             { RET(NEQ); }
    "{"             { RET('{'); }
    "}"             { RET('}'); }
    ","             { RET(','); }
    ":"             { RET(':'); }
    "="             { RET('='); }
    "("             { RET('('); }
    ")"             { RET(')'); }
    "["             { RET('['); }
    "]"             { RET(']'); }
    "."             { RET('.'); }
    "&"             { RET('&'); }
    "!"             { RET('!'); }
    "-"             { RET('-'); }
    "+"             { RET('+'); }
    "*"             { RET('*'); }
    "/"             { RET('/'); }
    "%"             { RET('%'); }
    "<"             { RET('<'); }
    ">"             { RET('>'); }
    "^"             { RET('^'); }
    "|"             { RET('|'); }
    "$"             { RET('$'); }
    "#"             { RET('#'); }

    [ \t\v\f]+      { goto std; }

    "\n"
    {
        if(cursor == s->eof) RET(EOI);
        s->line++;
        s->pos = cursor;
        RET(EOL);
    }
    
    any
    {
        printf("%s:%d:%d: warning: unexpected character %c (0x%02X)\n",
            s->filename, first_line, first_column, *s->tok, *s->tok);
        goto std;
    }
*/

linecomment:
/*!re2c
    "\n"
    {
        if (cursor == s->eof) RET(EOI);
        s->line++;
        s->pos = cursor;
        s->tok = cursor - 1;
        RET(EOL);
    }
    
    any             { goto linecomment; }
*/

comment:
/*!re2c
    "*""/"          { goto std; }
    "\n"
    {
        if(cursor == s->eof) {
            printf("%s:%d:%d: warning: open block comment at end of file\n",
                s->filename, first_line, first_column);
            RET(EOI);
        }
        s->tok = s->pos = cursor;
        s->line++;
        goto comment;
    }
    any             { goto comment; }
*/

retpoint:
    s->cur = cursor;
    if (loc) {
        loc->first_line = first_line;
        loc->num_lines = (unsigned short)(s->line - first_line);
        loc->first_column = first_column;
        loc->num_columns = (unsigned short)(s->cur - s->tok);
    }
    
    return tokenid;
}

Scanner *scanner_init(const char *fn) {
	FILE *file = fopen(fn, "r");
	Scanner *s;
    /*int fd = open(fn, O_RDONLY);*/
    
	/*if (fd < 0) {*/
    if (!file) {
        return NULL;
    }
    
    s = (Scanner *)malloc(sizeof(struct Scanner));
    memset(s, 0, sizeof(struct Scanner));
    /*s->fd = fd;*/
	s->file = file;
    s->line = 1;
    s->filename = fn;
    
    return s;
}

void scanner_deinit(Scanner *s) {
    /*close(s->fd);*/
	fclose(s->file);
    if (s->bot)
        free(s->bot);
    free(s);
}

Token *token_from_scanner(const Scanner *scanner, unsigned id,
        const SourceLocation *loc) {
    
    /* note: Token already contains space for the null terminator. */
    size_t textlen = (size_t)(scanner->cur - scanner->tok);
    Token *tok = (Token *)malloc(sizeof(struct Token) + textlen);
    tok->id = id;
    tok->loc = *loc;
    tok->next = 0;
    tok->child = 0;
    memcpy(tok->text, scanner->tok, textlen);
	tok->text[textlen] = 0;
    return tok;
}

Token *token_from_text(const char *text, unsigned id) {
    size_t textlen = (size_t)strlen(text);
    Token *tok = (Token *)malloc(sizeof(struct Token) + textlen);
    memset(tok, 0, sizeof(struct Token));
    tok->id = id;
    memcpy(tok->text, text, textlen + 1);
    return tok;
}

Token *token_free(Token *token) {
    Token *next = token->next, *child = token->child;
    free(token);
    if (child)
        token_free_all(child);
    return next;
}

void token_free_all(Token *token) {
    Token *next = token->next, *child = token->child;
    free(token);
    if (child)
        token_free_all(child);
    if (next)
        token_free_all(next);
}

