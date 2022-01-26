/*
File name			: parser.c
Compiler			: GCC
Author				: Johnathan Mangos
Course				: CST 8152 - Compilers, Lab Section: 012
Assignment			: 3
Date				: December 11th, 2019
Professor			: Sv. Ranev
Purpose				: Create a parser to enforce the syntatic grammar of Platypud
Function list		: parser(); match(); syn_eh(); syn_printe(); gen_incode(); program(); opt_statements();
					  statements(); statement(); statements_prime(); assignment_statement(); assignment_expression();
					  selection_statement(); iteration_statement(); pre_condition(); input_statement(); variable_list();
					  variable_list_prime(); variable_identifier(); output_statement(); output_list(); input_list(); arithmetic_expression();
					  unary_arithmetic_expression(); additive_arithmetic_expression(); additive_arithmetic_expression_prime();
					  multiplicative_arithmetic_expression(); multiplicative_arithmetic_expression_prime();
					  primary_arithmetic_expression(); string_expression(); string_expression_prime(); primary_string_expression();
					  conditional_expression(); logical_OR_expression(); logical_OR_expression_prime(); logical_AND_expression();
					  logical_AND_expression_prime(); relational_expression();

*/

#include "parser.h"
#include <stdlib.h>

/*
Purpose				: The function is to parse PLATYPUS program
Author				: Svillen Ranev
Versions			: 1.0
Called Functions	: none
Parameters			: none
Return value		: none
Algorithm			: - malar_next_token() then parse
					  - check SEOF
*/
void parser(void)
{
	lookahead = malar_next_token();
	program();
	match(SEOF_T, SEOF_0);
	gen_incode("PLATY: Source file parsed");
}

/*
Purpose				: The function is match current token and the token required by parser and check if they are match.
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: syn_eh(); malar_next_token(); syn_printe()
Parameters			: pr_token_code pr_token_attribute:
Return value		: none
Algorithm			: 
*/
void match(int pr_token_code, int pr_token_attribute)
{

	if (lookahead.code != pr_token_code) /* unsuccessful match */
	{
		syn_eh(pr_token_code);
		return;
	}

	if (lookahead.code == SEOF_T) /* Match succesful */
	{
		return;
	}

	switch (pr_token_code)
	{
	case KW_T:
	case ART_OP_T:
	case REL_OP_T:
	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int)
		{
			syn_eh(pr_token_code);
			return;
		}
		break;
	}

	/*match and successful */
	lookahead = malar_next_token();

	if (lookahead.code == ERR_T) /* handle error */
	{
		syn_printe();
		lookahead = malar_next_token();
		synerrno++;
		return;
	}
}

/*
 Purpose		    :  Panic recovery
 Author				: Johnathan Mangos
 Versions			: 1.0
 Called Functions	: syn_printe(); malar_next_token();
 Parameters			: sync_token_code
 Return value		: none
 Algorithm			: - calls syn_printe()
					  - Advance token to proper syntax or SEOF
					 */
void syn_eh(int sync_token_code)
{

	syn_printe();
	synerrno++;
	do
	{
		lookahead = malar_next_token();
		if (lookahead.code == SEOF_T)
		{
			if (sync_token_code != SEOF_T)
				exit(synerrno);
			else
			{
				return;
			}
		}
	} while (lookahead.code != sync_token_code);

	lookahead = malar_next_token();
	return;
}

/*
Purpose		        : error printing function
Author				: Sv. Ranev
Versions			: 1.0
Called Functions	: printf(); 
Parameters			: none
Return value		: none
*/

void syn_printe()
{
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code)
	{
	case ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case SVID_T: /* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T: /* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case ASS_OP_T: /* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case ART_OP_T: /* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case LOG_OP_T: /*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}
}

/*
 Purpose		    : Parser print function
 Author				: Johnathan Mangos
 Versions			: 1.0
 Called Functions	: none
 Parameters			: char
 */

void gen_incode(char *ch)
{
	printf("%s\n", ch);
}

/*
Purpose				: <program> -> PLATYPUS {<opt_statements>}
Author				: Sv. Ranev
*/

void program(void)
{
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
Purpose				: <opt_statements> - > <statements> | e
Author				: Sv. Ranev
*/

void opt_statements(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.get_int == READ ||
			lookahead.attribute.get_int == WRITE ||
			lookahead.attribute.get_int == IF ||
			lookahead.attribute.get_int == WHILE)
		{
			statements();
			break;
		}
	default:
		gen_incode("PLATY: opt_statements parsed");
	}
}

/*
Purpose				: <statements> -> <statement><statements'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: statement(); statements_prime();
*/
void statements(void)
{
	statement();
	statements_prime();
}

/*
Purpose				: <statement> -> <assignment statement>|<selection statement>|<iteration statement>|<input statement>|<output statement>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: assignment_statement(); selection_statement(); iteration_statement(); input_statement(); output_statement(); syn_printe();
*/

void statement(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		assignment_statement();
		break;
	case KW_T:
		switch (lookahead.attribute.get_int)
		{
		case IF:
			selection_statement();
			break;
		case WHILE:
			iteration_statement();
			break;
		case READ:
			input_statement();
			break;
		case WRITE:
			output_statement();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}
}

/*
Purpose				: <statements'> -> <statement><statements'> | e
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: statements();
*/

void statements_prime(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		statement();
		statements_prime();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE && lookahead.attribute.get_int != REPEAT && lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE)
		{
			statement();
			statements_prime();
			break;
		}
		break;
	}
}

/*
Purpose				: <assignment statement> -> <assignment expression>;
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: assignment_expression(); match(); gen_incode();
*/

void assignment_statement(void)
{
	assignment_expression();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
Purpose				: <assignment expression> -> AVID = <arithmetic expression> | SVID = <string expression>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); arithmetic_expression(); string_expression(); gen_incode(); syn_printe();
*/

void assignment_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		arithmetic_expression();
		gen_incode("PLATY: Assignment expression parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, NO_ATTR);
		string_expression();
		gen_incode("PLATY: Assignment expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}

/*
Purpose				: <selection statement> ->
					  IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> }
					  ELSE { <opt_statements> } ;
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); pre_condition(); conditional_expression(); opt_statements(); gen_incode();
*/

void selection_statement(void)
{
	match(KW_T, IF);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Selection statement parsed");
}

/*
Purpose				: <iteration statement> ->
					  WHILE <pre-condition> (<conditional expression>)
					  REPEAT { <statements>};
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); pre_condition(); conditional_expression(); statements(); gen_incode();
*/
void iteration_statement(void)
{
	match(KW_T, WHILE);
	pre_condition();
	match(LPR_T, NO_ATTR);
	conditional_expression();
	match(RPR_T, NO_ATTR);
	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);
	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Iteration statement parsed");
}

/*
Purpose				: <pre-condition> -> TRUE | FALSE
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); syn_printe();
*/

void pre_condition(void)
{
	switch (lookahead.code)
	{
	case KW_T:
		switch (lookahead.attribute.get_int)
		{
		case TRUE:
			match(KW_T, TRUE);
			break;
		case FALSE:
			match(KW_T, FALSE);
			break;
		default:
			syn_printe();
			break;
		}
		break;
	default:
		syn_printe();
		break;
	}
}

/*
Purpose				: <input statement> -> READ (<input_list>);
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); variable_list(); gen_incode();
*/

void input_statement(void)
{
	match(KW_T, READ);
	match(LPR_T, NO_ATTR);
	input_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}
void input_list(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Input list parsed");
		break;
	default:
		gen_incode("PLATY: Input list parsed");
		break;
	}
}

/*
Purpose				: <variable list> -> <variable identifier> <variable list'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: variable_identifier(); variable_list_prime(); gen_incode();
*/

void variable_list(void)
{
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

/*
Purpose				: <variable list'> -> ,<variable identifier> <variable list'> |  e
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); variable_identifier(); variable_list_prime();
*/

void variable_list_prime(void)
{
	switch (lookahead.code)
	{
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
		break;
	}
}

/*
Purpose				: <variable identifier> -> AVID | SVID
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); syn_printe();
*/

void variable_identifier(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*
Purpose				: <output statement> -> WRITE (<output_list>);
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); output_list(); gen_incode();
*/
void output_statement(void)
{
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*
Purpose				: <output_list> -> <opt_variable list> | STR_T;
Author				: Johnathan Mangos

Versions			: 1.0
Called Functions	: variable_list(); match(); gen_incode();
*/
void output_list(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list parsed");
		break;
	default:
		gen_incode("PLATY: Output list parsed");
		break;
	}
}

/*
Purpose				: <arithmetic expression> - > <unary arithmetic expression>|<additive arithmetic expression>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: unary_arithmetic_expression(); gen_incode(); syn_printe(); additive_arithmetic_expression();
*/

void arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			unary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_expression();
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Arithmetic expression parsed");
}

/*
Purpose				: <unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); primary_arithmetic_expression(); gen_incode(); syn_printe();
*/
void unary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
			match(ART_OP_T, PLUS);
			primary_arithmetic_expression();
			break;
		case MINUS:
			match(ART_OP_T, MINUS);
			primary_arithmetic_expression();
			break;
		default:
			syn_printe();
			break;
		}
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}

/*
Purpose				: <additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: multiplicative_arithmetic_expression(); additive_arithmetic_expression_prime();
*/

void additive_arithmetic_expression(void)
{
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_prime();
}

/*
Purpose				: <additive arithmetic expression'> ->
						+ <multiplicative arithmetic expression><additive arithmetic expression'>
					  | -  <multiplicative arithmetic expression><additive arithmetic expression'>
					  | e
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); additive_arithmetic_expression(); gen_incode();
*/

void additive_arithmetic_expression_prime(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case PLUS:
		case MINUS:
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
			break;
		default:
			break;
		}
	}
}

/*
Purpose				: <multiplicative arithmetic expression> -> <primary arithmetic expression><multiplicative arithmetic expression'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: primary_arithmetic_expression(); multiplicative_arithmetic_expression_prime();
*/

void multiplicative_arithmetic_expression(void)
{
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
}

/*
Purpose				: <multiplicative arithmetic expression'> ->
						* <primary arithmetic expression><multiplicative arithmetic expression'>
					  | / <primary arithmetic expression><multiplicative arithmetic expression'>
					  | e
Author				: Johnathan Mangos

Versions			: 1.0
Called Functions	: match(); primary_arithmetic_expression(); multiplicative_arithmetic_expression_prime(); gen_incode()
*/

void multiplicative_arithmetic_expression_prime(void)
{
	switch (lookahead.code)
	{
	case ART_OP_T:
		switch (lookahead.attribute.arr_op)
		{
		case MULT:
		case DIV:
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
			break;
		default:
			break;
		}
	}
}

/*
Purpose				: <primary arithmetic expression> ->
						AVID_T | FPL_T | INL_T | (<arithmetic expression>)
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); gen_incode(); arithmetic_expression(); syn_printe();
*/

void primary_arithmetic_expression(void)
{
	switch (lookahead.code)
	{
	case AVID_T:
	case FPL_T:
	case INL_T:
		match(lookahead.code, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_expression();
		match(RPR_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
Purpose				: <string expression> -> <primary string expression> <string expression'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: primary_string_expression(); string_expression_prime(); gen_incode();
*/

void string_expression(void)
{
	primary_string_expression();
	string_expression_prime();
	gen_incode("PLATY: String expression parsed");
}

/*
Purpose				: <string expression'> -> << <primary string expression> <string expression'> | e
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: primary_string_expression(); string_expression_prime(); match();
*/
void string_expression_prime(void)
{
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_expression();
		string_expression_prime();
		break;
	default:
		break;
	}
}

/*
Purpose				: <primary string expression> ->  SVID_T | STR_T
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); syn_printe(); gen_incode();
*/

void primary_string_expression(void)
{
	switch (lookahead.code)
	{
	case SVID_T:
	case STR_T:
		match(lookahead.code, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
Purpose				: <conditional expression> -> <logical OR  expression>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: logical_OR_expression(); gen_incode();
*/
void conditional_expression(void)
{
	logical_OR_expression();
	gen_incode("PLATY: Conditional expression parsed");
}
/*
Purpose				: <logical OR expression> -> <logical AND expression> <logical OR expression'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: logical_AND_expression(); logical_OR_expression_prime();
*/

void logical_OR_expression(void)
{
	logical_AND_expression();
	logical_OR_expression_prime();
}

/*
Purpose				: <logical OR expression'> -> .OR.  <logical AND expression><logical OR expression'> | e
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); logical_AND_expression(); logical_OR_expression_prime(); gen_incode();
*/

void logical_OR_expression_prime(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case OR:
			match(LOG_OP_T, OR);
			logical_AND_expression();
			logical_OR_expression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
		break;
	}
}

/*
Purpose				: <logical AND expression> -> <relational expression> <logical AND expression'>
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: relational_expression(); logical_AND_expression_prime();
*/

void logical_AND_expression(void)
{
	logical_AND_expression_prime();
}

/*
Purpose				: <logical AND expression'> -> .AND. <relational expression><logical AND expression'> | e
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	: match(); relational_expression(); logical_AND_expression_prime(); gen_incode();
*/

void logical_AND_expression_prime(void)
{
	switch (lookahead.code)
	{
	case LOG_OP_T:
		switch (lookahead.attribute.log_op)
		{
		case AND:
			match(LOG_OP_T, AND);
			logical_AND_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
			break;
		}
		break;
	}
}

/*
Purpose				: <relational expression> ->
Author				: Johnathan Mangos
Versions			: 1.0
Called Functions	:
*/

void relational_expression(void)
{

	gen_incode("PLATY: Relational expression parsed");
}