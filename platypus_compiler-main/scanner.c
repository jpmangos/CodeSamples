/* File name: scanner.c
* Compiler: gcc
* Author:Johnathan Mangos, S/N: 040902701
* Course:CST 8152 –Compilers
* Lab Section: 012
* Assignment: 2
* Date: Wednesday, November 24th, 2019
* Professor: Sv.Ranev
* Purpose:To create a scanner
* Function list:aa_funcxx(many), strlen, strcmp, strcpy, 
isdigit, isspace, isalpha, isalnum, b_getc, b_getcoffset, b_mark, b_limit, b_allocate, b_compact
b_reset, get_next_state, b_retract, char_clas and b_free */

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>  /* standard input / output */
#include <ctype.h>  /* conversion functions */
#include <stdlib.h> /* standard library functions and constants */
#include <string.h> /* string functions */
#include <limits.h> /* integer types constants */
#include <float.h>  /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h> /* assert() prototype */
/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG /* for conditional processing */
#undef DEBUG
/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line;                /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf; /*pointer to temporary lexeme buffer*/
static pBuffer sc_buf;  /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c);         /* character class function */
static int get_next_state(int, char);  /* state machine function */
static int iskeyword(char *kw_lexeme); /*keywords lookup functuion */

/*Initializes scanner */
int scanner_init(pBuffer psc_buf)
{
  if (b_isempty(psc_buf))
    return EXIT_FAILURE; /*1*/
  /* in case the buffer has been read previously  */
  b_rewind(psc_buf);
  b_clear(str_LTBL);
  line = 1;
  sc_buf = psc_buf;
  return EXIT_SUCCESS;   /*0*/
  /*   scerrnum = 0;  */ /*no need - global ANSI C */
}
/*Purpose: Match and return a lexeme token
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: isdigit, isspace, isalpha, isalnum, b_getc, b_getcoffset, b_mark, b_limit, b_allocate, b_compact
b_reset, get_next_state, b_retract and b_free
* Parameters: none
* Return value: t
* Algorithm: Check for special case tokens, if none are found intialize the DFA and match for lexemes.*/
Token malar_next_token(void)
{
  {
    Token t = {0};   /* token to return after pattern recognition. Set all structure members to 0 */
    unsigned char c; /* input symbol */
    int state = 0;   /* initial state of the FSM */
    short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
    short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/
    int i;

    while (1)
    { /* endless loop broken by token returns it will generate a warning */

      c = b_getc(sc_buf);

      /* Part 1: Implementation of token driven scanner */
      /* every token is possessed by its own dedicated code */

      if (isspace(c))
      {
        if (c == '\n') /* Handle spaces and line numbers */
        {
          line++;
        }
        continue;
      }
      switch (c) /*Switch statement handles special operators.*/
      {
      case SEOF:
        t.code = SEOF_T;
        t.attribute.seof = SEOF_0;
        return t;
      /*Seperators*/
      case '(':
        t.code = LPR_T;
        return t;
      case ')':
        t.code = RPR_T;
        return t;
      case '{':
        t.code = LBR_T;
        return t;
      case '}':
        t.code = RBR_T;
        return t;
      case ',':
        t.code = COM_T;
        return t;
      case ';':
        t.code = EOS_T;
        return t;
      /*Relational operators and String concatenation*/
      case '<':
        if (b_getc(sc_buf) == '<')
        {
          t.code = SCC_OP_T;
          return t;
        }
        else if (b_getc(sc_buf) == '>')
        {
          t.code = REL_OP_T;
          t.attribute.rel_op = NE;
          return t;
        }
        else
        {
          t.code = REL_OP_T;
          t.attribute.rel_op = LT;
          b_retract(sc_buf);
          return t;
        }
      case '>':
        t.code = REL_OP_T;
        t.attribute.rel_op = GT;
        return t;
      case '=':
        if (b_getc(sc_buf) == '=')
        {
          t.code = REL_OP_T;
          t.attribute.rel_op = EQ;
          return t;
        }
        /* Assignment operator */
        else
        {
          t.code = ASS_OP_T;
          b_retract(sc_buf);
          return t;
        }
      /*Arithmitic operators*/
      case '+':
        t.code = ART_OP_T;
        t.attribute.arr_op = PLUS;
        return t;
      case '-':
        t.code = ART_OP_T;
        t.attribute.arr_op = MINUS;
        return t;
      case '*':
        t.code = ART_OP_T;
        t.attribute.arr_op = MULT;
        return t;
      case '/':
        t.code = ART_OP_T;
        t.attribute.arr_op = DIV;
        return t;
      /* Logical operators */
      case '.':
        b_mark(sc_buf, b_getcoffset(sc_buf)); /* Set the mark because we need to  read multiple chars*/
        /*Check for .AND. */
        if (b_getc(sc_buf) == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.')
        {
          t.code = LOG_OP_T;
          t.attribute.log_op = AND;
          return t;
        }
        else
        {
          b_reset(sc_buf); /* Reset on fail */
        }
        /* Check for .OR. */
        if (b_getc(sc_buf) == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.')
        {
          t.code = LOG_OP_T;
          t.attribute.log_op = OR;
          return t;
        }
        /* This is an error - Meaning not a logical operator or an invalid literal declaration */
        else
        {
          b_reset(sc_buf);
          t.code = ERR_T;
          t.attribute.err_lex[0] = '.';
          t.attribute.err_lex[1] = '\0';
          return t;
        }
      /*Comments*/
      case '!':
        c = b_getc(sc_buf);
        if (c != '!')
        {
          t.code = ERR_T;
          t.attribute.err_lex[0] = '!';
          t.attribute.err_lex[1] = c;
          t.attribute.err_lex[2] = '\0';
          while (c != '\n') /* loop until end of line increment line or find SEOF */
          {
            c = b_getc(sc_buf);
            if (c == '\n')
            {
              line++;
            }
            if (c == SEOF)
            {
              t.code = SEOF_T;
              t.attribute.seof = SEOF_EOF;
              return t;
            }
          }
          return t;
        }
        else
        {
          while (c != '\n')
          {
            c = b_getc(sc_buf);
            if (c == '\n')
            {
              line++;
            }
            if (c == SEOF)
            {
              t.code = SEOF_0;
              t.attribute.seof = SEOF_0;
              return t;
            }
          }
          continue;
        }

      default:
        break;
      }

      /* Part 2: Implementation of Finite State Machine (DFA)
           or Transition Table driven Scanner 
           Note: Part 2 must follow Part 1 to catch the illegal symbols*/

      if (isalnum(c) || c == '"')
      {
        lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1); /*Retract the mark so the next get_c can be properly identified. */
        state = get_next_state(state, c);                    /* Check if this works better in the loop */

        while (as_table[state] == NOAS) /* Loop until we find an acceptig state */
        {
          c = b_getc(sc_buf);
          state = get_next_state(state, c);
        }
        lexend = b_getcoffset(sc_buf);
        if (as_table[state] == ASWR)
        {
          lexend -= 1;
          b_retract(sc_buf);
        }
        lex_buf = b_allocate(lexend - lexstart, 0, 'f');
        if (lex_buf == NULL)
        {
          scerrnum = 1; /* sets exit(1) in play_pt.c */
          t.code = ERR_T;
          strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
          return t;
        }
        b_reset(sc_buf);

        for (i = lexstart; i < lexend; i++)
        {
          c = b_getc(sc_buf);
          b_addc(lex_buf, c);
        }

        b_compact(lex_buf, '\0');
        t = aa_table[state](b_location(lex_buf)); /*This is a bit tricky but you set the function with the state 
                                                 and b_location point to the beginning of the array because we used calloc in the buffer setting the
                                                 mark to 0.*/
        b_free(lex_buf);
        return t;
      }
      else /* this handles invalid symbols like '%, and '$' */
      {
        if (isspace(c))
        {
          if (c == '\n')
            line++;
          continue;
        }
        else
        {
          t.code = ERR_T;
          t.attribute.err_lex[0] = c;
          t.attribute.err_lex[1] = '\0';
          return t;
        }
      }
    }
  }
}
/*Purpose:
* Author: Sv. Ranev
* History/Versions: 1.0 Nov 24th
* Called functions: 
* Parameters:
* Return value: 
* Algorithm: */
int get_next_state(int state, char c)
{
  int col;
  int next;
  col = char_class(c);
  next = st_table[state][col];
#ifdef DEBUG
  printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

  assert(next != IS);

#ifdef DEBUG
  if (next == IS)
  {
    printf("Scanner Error: Illegal state:\n");
    printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
    exit(1);
  }
#endif
  return next;
}
/*Purpose: Gets the state of the char, and returns the next step of DFA
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: Isdigit, isalpha
* Parameters: char
* Return value: int 0 - 7
* Algorithm: Compare the column value to the char and match appropriate state*/
int char_class(char c)
{
  int val;
  /* [a-zA-Z]*/
  if (isalpha(c))
  {
    val = 0;
  }
  /* 0 */
  else if (c == '0')
  {
    val = 1;
  }
  /* [1-9] */
  else if (c != '0' && isdigit(c))
  {
    val = 2;
  }
  /* The rest of these are pretty self explanatory and in order.*/
  else if (c == '.')
  {
    val = 3;
  }
  else if (c == '@')
  {
    val = 4;
  }
  else if (c == '"')
  {
    val = 5;
  }
  else if (c == SEOF)
  {
    val = 6;
  }
  /* other */
  else
  {
    val = 7;
  }
  return val;
}
/*Purpose: AVID accepting function
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: strlen, strcpy, iskeyword
* Parameters: lexeme
* Return value: t
* Algorithm: Check AVID length and save lexeme*/
Token aa_func02(char lexeme[])
{
  Token t = {0};
  int i;             /* index/iterator */
  int keyword_index; /* Keyword index for selecting from the table */
#ifdef DEBUG
  printf("lexeme: |%s|\n", lexeme);
#endif
  keyword_index = iskeyword(lexeme);
  if (keyword_index != -1)
  {
    t.code = KW_T;
    t.attribute.kwt_idx = keyword_index;
    return t;
  }
  else
  {
    t.code = AVID_T;
    if (strlen(lexeme) > VID_LEN)
    {
      for (i = 0; i < VID_LEN; i++)
      {
        t.attribute.vid_lex[i] = lexeme[i];
      }
      t.attribute.vid_lex[VID_LEN] = '\0';
    }
    else
    {
      strcpy(t.attribute.vid_lex, lexeme);
    }
  }
  return t;
}
/*Purpose: SVID accepting function
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: strlen, strcpy
* Parameters: lexeme
* Return value: t
* Algorithm: Check SVID length and save lexeme*/
Token aa_func03(char lexeme[])
{
  Token t = {0};
  int i; /* index/iterator */

  t.code = SVID_T;
  if (strlen(lexeme) > VID_LEN)
  {
    for (i = 0; i < VID_LEN - 1; i++)
    {
      t.attribute.vid_lex[i] = lexeme[i];
    }
    t.attribute.vid_lex[VID_LEN - 1] = '@';
    t.attribute.vid_lex[VID_LEN] = '\0';
  }
  else
  {
    strcpy(t.attribute.vid_lex, lexeme);
  }

  return t;
}
/*Purpose: FPL literal accepting function
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: atol, strlen, strcpy
* Parameters: lexeme 
* Return value: t
* Algorithm: You have to do a range check in this function to ensure no errors*/
Token aa_func08(char lexeme[])
{
  Token t = {0};
  double range_check; /* Float holding variable */
  int i;
  range_check = atof(lexeme); /* Iterator */

  if (range_check > FLT_MAX || range_check < FLT_DIG) /* Check limits */
  {
    t.code = ERR_T;
    if (strlen(lexeme) > ERR_LEN) /* Long error */
    {
      for (i = 0; i < ERR_LEN - 3; i++)
      {
        t.attribute.err_lex[i] = lexeme[i];
        t.attribute.err_lex[ERR_LEN - 3] = '.';
        t.attribute.err_lex[ERR_LEN - 2] = '.';
        t.attribute.err_lex[ERR_LEN - 1] = '.';
        t.attribute.err_lex[ERR_LEN] = '\0';
      }
      return t;
    }
    else
    {
      strcpy(t.attribute.err_lex, lexeme); /* Short error */
      return t;
    }
  }
  else
  {
    t.code = FPL_T; /* Valid */
    t.attribute.flt_value = (float)range_check;
  }

  return t;
}
/*Purpose: Interger literal accepting function
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: atol, strlen, strcpy
* Parameters: lexeme 
* Return value: t
* Algorithm: You have to do a range check in this function to ensure no errors*/
Token aa_func05(char lexeme[])
{

  Token t = {0};
  long range_check; /* Short holding variable */
  int i;
  range_check = atol(lexeme);

  if (range_check > 0 && lexeme[0] == '0')
  {
    t = aa_table[ES](lexeme);
  }
  else if (range_check > SHRT_MAX || range_check < SHRT_MIN) /* Check limits */
  {
    t.code = ERR_T;
    if (strlen(lexeme) > ERR_LEN) /* Long error */
    {
      for (i = 0; i < ERR_LEN - 3; i++)
      {
        t.attribute.err_lex[i] = lexeme[i];
        t.attribute.err_lex[ERR_LEN - 3] = '.';
        t.attribute.err_lex[ERR_LEN - 2] = '.';
        t.attribute.err_lex[ERR_LEN - 1] = '.';
        t.attribute.err_lex[ERR_LEN] = '\0';
      }
      return t;
    }
    else
    {
      strcpy(t.attribute.err_lex, lexeme); /* Short error */
      return t;
    }
  }
  else
  {
    t.code = INL_T; /* Valid */
    t.attribute.int_value = (short)range_check;
  }

  return t;
}
/*Purpose: String literal accepting function
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: strlen, b_limit, b_addc, b_compact
* Parameters: lexeme
* Return value: t
* Algorithm: Ignore quotes and save the lexeme into the string literal table and compact*/
Token aa_func10(char lexeme[])
{

  Token t = {0};
  int i; /* iterator */

  t.attribute.str_offset = b_limit(str_LTBL); /* Starting addc_offset is the start of SL */
  for (i = 0; i < (int)strlen(lexeme); i++)
  {

    if (lexeme[i] != '"') /* Check Quotes and copy */
    {
      b_addc(str_LTBL, lexeme[i]);
    }
    if (lexeme[i] == '\n')
    {
      line++;
    }
  }
  /*We compact here because because it isn't done in the main program,
    and it has the added of bonus of adding the '\0' terminator */
  b_compact(str_LTBL, '\0');
  t.code = STR_T;

  return t;
}
/*Purpose: Error handling function
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: strlen, strcpy
* Parameters: lexeme
* Return value: t
* Algorithm: Check line numbers*/
Token aa_func11(char lexeme[])
{
  Token t = {0};
  int i; /*Iterator*/
  t.code = ERR_T;
  if (strlen(lexeme) > ERR_LEN) /* Long error */
  {
    for (i = 0; i < ERR_LEN - 3; i++)
    {
      t.attribute.err_lex[i] = lexeme[i];
    }
    t.attribute.err_lex[ERR_LEN - 3] = '.';
    t.attribute.err_lex[ERR_LEN - 2] = '.';
    t.attribute.err_lex[ERR_LEN - 1] = '.';
    t.attribute.err_lex[ERR_LEN] = '\0';
    return t;
  }
  else
  {
    strcpy(t.attribute.err_lex, lexeme); /* Short error */
  }
  for (i = 0; i < (int)strlen(lexeme); i++) /* check line increment*/
  {
    if (lexeme[i] == '\n')
    {
      line++;
    }
  }
  return t;
}
/*Purpose: Redirect to the ES function. Kind of unneccesary but we did it to match the table
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: aa_table[ES]
* Parameters: lexeme
* Return value: t
* Algorithm: Redirect output to ES*/
Token aa_func12(char lexeme[])
{
  Token t = {0};
  t = aa_table[ES](lexeme);
  return t;
}
/*Purpose: Match the lexeme to Token
* Author: Johnathan Mangos
* History/Versions: 1.0 Nov 24th
* Called functions: 
* Parameters: strcmp
* Return value: -1 to 9
* Algorithm: compare the lexeme against the array of keywords*/
int iskeyword(char *kw_lexeme)
{
  int i;
  for (i = 0; i < KWT_SIZE; i++)
  {
    if (strcmp(kw_lexeme, kw_table[i]) == 0)
    {
      return i;
    }
  }
  return -1;
}