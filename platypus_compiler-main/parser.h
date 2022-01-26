/*
File name			: parser.h
Compiler			: GCC
Author				: Johnathan Mangos
Course				: CST 8152 - Compilers, Lab Section: 012
Assignment			: 3
Date				: December 11th, 2019
Professor			: Sv. Ranev
*/

#ifndef TOKEN_H_
#include "token.h"
#endif

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#define NO_ATTR -1
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

Token lookahead; /* input token */
int synerrno;    /* syntax errors */

/* externs */
extern Token malar_next_token(void);
extern int line;
extern Buffer *str_LTBL;
extern char *kw_table[];

/* function decls. */
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gen_incode(char *ch);
void program(void);
void opt_statements(void);
void statements(void);
void statement(void);
void statements_prime(void);
void assignment_statement(void);
void assignment_expression(void);
void selection_statement(void);
void iteration_statement(void);
void pre_condition(void);
void input_statement(void);
void variable_list(void);
void variable_list_prime(void);
void variable_identifier(void);
void output_statement(void);
void output_list(void);
void input_list(void);
void arithmetic_expression(void);
void unary_arithmetic_expression(void);
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void primary_arithmetic_expression(void);
void string_expression(void);
void string_expression_prime(void);
void primary_string_expression(void);
void conditional_expression(void);
void logical_OR_expression(void);
void logical_OR_expression_prime(void);
void logical_AND_expression(void);
void logical_AND_expression_prime(void);