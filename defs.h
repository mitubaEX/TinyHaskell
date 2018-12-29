#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE	1
#define FALSE	0

enum {CONS, NODE, LEAF};

typedef struct cell {
	int          kind;
	struct cell *head;
	struct cell *tail;
	struct cell *args;
} Cell;

extern char *yytext;
extern int linecounter;

int main(void);
int yylex(void);
void comment(void);
int yyparse(void);
void yyerror(char*);
Cell *cons(Cell *, Cell *);
Cell *cons_rfunc(Cell *, Cell *, Cell *);
Cell *cons_lfunc(Cell *, Cell *, Cell *);
Cell *cons_func_asign_func(Cell *, Cell *, Cell *, Cell *);
Cell *node(char *, Cell *);
Cell *leaf(char *, char *);
void tree(Cell *);
void visit(Cell *, int);

