#include "defs.h"
#include <string.h>

int main(void) {
	linecounter = 1;
	if (yyparse() == 0) {
		fprintf(stderr, "\nparser successfully ended\n\n");
	}
	return(EXIT_SUCCESS);
}

Cell *cons(Cell *car, Cell *cdr) {
	Cell *pointer;

	pointer = (Cell *)malloc(sizeof(Cell));
	pointer->kind = CONS;
	pointer->head = car;
	pointer->tail = cdr;
	pointer->args = NULL;
	return(pointer);
}

Cell *cons_func(Cell *car, Cell *args, Cell *cdr) {
	// set args
	car->args = args;

	Cell *pointer;

	pointer = (Cell *)malloc(sizeof(Cell));
	pointer->kind = CONS;
	pointer->head = car;
	pointer->tail = cdr;
	pointer->args = NULL;
	return(pointer);
}

Cell *node(char *car, Cell *cdr) {
	Cell *pointer;

	pointer = (Cell *)malloc(sizeof(Cell));
	pointer->kind = NODE;
	pointer->head = (Cell *)strdup(car);
	pointer->tail = cdr;
	pointer->args = NULL;
	return(pointer);
}

Cell *leaf(char *car, char *cdr) {
	Cell *pointer;

	pointer = (Cell *)malloc(sizeof(Cell));
	pointer->kind = LEAF;
	pointer->head = (Cell *)strdup(car);
	pointer->tail = (Cell *)strdup(cdr);
	pointer->args = NULL;
	return(pointer);
}

void tree(Cell *pointer) {
	visit(pointer, 1);
	printf("\n-----------------------------\n");
}

void args_visit(Cell *pointer, int level) {
	if (pointer->kind == CONS) {
		args_visit(pointer->head, level + 1);
		args_visit(pointer->tail, level + 1);
	}
	if (pointer->kind == NODE) {
		args_visit(pointer->tail, level + 1);
	}
	if (pointer->kind == LEAF) {
		printf(" %s", (char *)pointer->tail);
	}
	return;
}

void visit(Cell *pointer, int level) {
	int count;

	printf("\n");
	for (count = 0; count < level; count++) {
		printf("    ");
	}
	if (pointer->kind == CONS) {
		printf("cons(");
		visit(pointer->head, level + 1);
		visit(pointer->tail, level + 1);
		printf(")");
	}
	if (pointer->kind == NODE) {
		printf("node(");
		printf("%s ", (char *)pointer->head);
		visit(pointer->tail, level + 1);
		printf(")");
	}
	if (pointer->kind == LEAF) {
		printf("leaf(");
		printf("%s ", (char *)pointer->head);
		printf("%s", (char *)pointer->tail);
		if (pointer->args != NULL) {
			args_visit(pointer->args, level + 1);
		}
		printf(")");
	}
	return;
}
