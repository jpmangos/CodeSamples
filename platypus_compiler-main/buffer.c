/*File name: Buffer.c
* Compiler: gcc
* Author:Johnathan Mangos, S/N: 040902701
* Course:CST 8152 –Compilers
* Lab Section: 012
* Assignment: 2
* Date: Wednesday, November 24th, 2019
* Professor: Sv.Ranev
* Purpose:To create a buffer capable of reading a file for use in scanner
* Function list: b_allocate, b_addc, b_clear, b_free , b_isfull, b_limit, 
* b_capacity, b_mark, b_mode, b_incfactor , b_load, b_isempty, b_getc, b_eob , b_print, b_compact, 
* b_rflag, b_retract, b_reset, b_getcoffset, b_location, b_rewind */

/* NOTE:1. I try to use self-documenting names for variables and functions, so I will not be overly commenting self-explanatory variable
        2. I will try to use common C best practices for standard function such as realloc so I will not be overly commenting those either */
#include <stdlib.h>
#include "buffer.h"

/* function declarations */

/*Purpose: To allocate the buffer
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: malloc, calloc
* Parameters: short init_capacity, char inc_factor, char o_mode 
* Return value: Buffer Pointer or NULL
* Algorithm: Check for valid parameters attempt to allocate a buffer and set the dependent variables*/
Buffer *b_allocate(short init_capacity, char inc_factor, char o_mode)
{
    pBuffer alloc_bufferptr; /* Buffer to returned if properly allocated */

    /* Just a question, but as far as I understand the implementation of "char" type is platform depedent so to get 
    the specific implementation of inc_factor we should use unsigned char, o_mode should use signed char on the other hand. 
    I'm not sure how to program this in a "Excessively Defensive" way without modifying the buffer descriptor, 
    or function parameters which would be a violation of the specification. My best guess is we explicitly cast inc_factor (Paramater) 
    to unsigned char, but I also don't know how the compiler will handle its assignment to inc_factor (Buffer Descriptor) after.
    Also, forcing the compiler to use a certain type is considered unsafe as well as I understand it.
    
    Feedback appreciated! */

    /* Parameter chacking logic set defaults, and return null on bad parameters*/
    if (init_capacity == 0)
    {
        init_capacity = DEFAULT_INIT_CAPACITY;
    }
    if (inc_factor == 0)
    {
        inc_factor = DEFAULT_INC_FACTOR;
    }
    if (init_capacity >= SHRT_MAX)
    {
        return NULL;
    }
    if (init_capacity < 0)
    {
        return NULL;
    }
    if (inc_factor < 0)
    {
        return NULL;
    }
    if (o_mode == 'a')
    {
        if ((unsigned int)inc_factor > 255 || inc_factor < 1)
        {
            return NULL;
        }
        alloc_bufferptr = (pBuffer)calloc(8, sizeof(pBuffer)); /* These are the kind of NULL checks I consider standard in C and will not be commented */
        if (alloc_bufferptr == NULL)
        {
            return NULL;
        }

        alloc_bufferptr->cb_head = (char *)malloc(sizeof(char) * init_capacity);

        if (alloc_bufferptr->cb_head == NULL)
        {
            return NULL;
        }
        else
        {
            alloc_bufferptr->capacity = init_capacity;
            alloc_bufferptr->addc_offset = 0;
            alloc_bufferptr->getc_offset = 0;
            alloc_bufferptr->mode = 1;
            alloc_bufferptr->inc_factor = inc_factor;
            alloc_bufferptr->flags = DEFAULT_FLAGS;
            return alloc_bufferptr;
        }
    }
    else if (o_mode == 'm')
    {
        /*parameter checking*/
        if (inc_factor > 100 || inc_factor < 1)
        {
            return NULL;
        }

        alloc_bufferptr = (pBuffer)calloc(8, sizeof(pBuffer));

        if (alloc_bufferptr == NULL)
        {
            return NULL;
        }

        alloc_bufferptr->cb_head = (char *)malloc(sizeof(char) * init_capacity);

        if (alloc_bufferptr == NULL)
        {
            return NULL;
        }
        else
        {
            alloc_bufferptr->capacity = init_capacity;
            alloc_bufferptr->addc_offset = 0;
            alloc_bufferptr->getc_offset = 0;
            alloc_bufferptr->mode = -1;
            alloc_bufferptr->inc_factor = inc_factor;
            alloc_bufferptr->flags = DEFAULT_FLAGS;
            return alloc_bufferptr;
        }
    }
    else if (o_mode == 'f')
    {
        alloc_bufferptr = (pBuffer)calloc(8, sizeof(pBuffer));
        if (alloc_bufferptr == NULL)
        {
            return NULL;
        }
        alloc_bufferptr->cb_head = (char *)malloc(sizeof(char) * init_capacity);
        if (alloc_bufferptr->cb_head == NULL)
        {
            return NULL;
        }
        else
        {
            alloc_bufferptr->capacity = init_capacity;
            alloc_bufferptr->addc_offset = 0;
            alloc_bufferptr->getc_offset = 0;
            alloc_bufferptr->mode = 0;
            alloc_bufferptr->inc_factor = 0;
            alloc_bufferptr->flags = DEFAULT_FLAGS;
            return alloc_bufferptr;
        }
    }
    else
    {
        return NULL;
    }
}
/*Purpose: Adds a character to the buffer and expands the buffer if it can't
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: realloc, b_isfull
* Parameters: Buffer pointer & char
* Return value: Buffer Pointer or NULL
* Algorithm: Add a character if the buffer is full expand or return null depending on the mode and SHRT_MAX*/
pBuffer b_addc(pBuffer const pBD, char symbol)
{
    if (pBD == NULL)
    {
        return NULL;
    }
    if (pBD->mode == 1)
    {

        if (b_isfull(pBD) == 1)
        {
            char *temp_array; /* Ever instance of temp_array serves to check reallocation and provide safety these will not be commented 
                                from now on.*/
            short new_capacity; /* Every instance of new capacity serves the purpose of maintaining the original capacity to prevent damaging the buffer
                                    these will not be commented */

            if (pBD->capacity == (SHRT_MAX - 1))
            {
                return NULL;
            }

            new_capacity = pBD->capacity + pBD->inc_factor;

            if (new_capacity < 0) /* Negative short means the max was exceeded */
            {
                new_capacity = SHRT_MAX - 1;
            }

            temp_array = (char *)realloc(pBD->cb_head, new_capacity * sizeof(char));
            if (temp_array == NULL)
            {
                printf("Could not reallocate memory");
                return NULL;
            }
            else
            {
                if (temp_array != pBD->cb_head)
                {
                    pBD->flags |= SET_R_FLAG;
                }
                pBD->cb_head = temp_array;
                pBD->capacity = new_capacity;
                pBD->cb_head[pBD->addc_offset] = symbol;
                pBD->addc_offset += 1;
                return pBD;
            }
        }
        else
        {
            pBD->flags &= RESET_R_FLAG; /* Unneccesary for Fixed-Mode, keeping in incase future assignment require mode switching */
            pBD->cb_head[pBD->addc_offset] = symbol;
            pBD->addc_offset += 1;
            return pBD;
        }
    }
    if (pBD->mode == -1)
    {
        if (b_isfull(pBD) == 1)
        {
            char *temp_array;
            short new_capacity;
            short available_space; /* These two variable serve to calculate the new capacity in fixed mode */
            short new_increment;
            
            if (pBD->capacity == (SHRT_MAX - 1))
            {
                return NULL;
            }
            available_space = SHRT_MAX - 1 - pBD->capacity;
            new_increment = (long)available_space * pBD->inc_factor / 100; /* A long cast to ensure correct values */
            new_capacity = pBD->capacity + new_increment;

            if (new_increment == 0) /* Division of a variable less then 100 yields 0 */
            {
                new_capacity = SHRT_MAX - 1;
            }
            if (new_capacity < 0) 
            {
                new_capacity = SHRT_MAX - 1;
            }

            temp_array = (char *)realloc(pBD->cb_head, new_capacity * sizeof(char));
            if (temp_array == NULL)
            {
                printf("Could not reallocate memory");
                return NULL;
            }
            else
            {
                if (temp_array != pBD->cb_head)
                {
                    pBD->flags |= SET_R_FLAG;
                }
                pBD->cb_head = temp_array;
                pBD->capacity = new_capacity;
                pBD->cb_head[pBD->addc_offset] = symbol;
                pBD->addc_offset += 1;
                return pBD;
            }
        }
        else
        {
            pBD->flags &= RESET_R_FLAG;
            pBD->cb_head[pBD->addc_offset] = symbol;
            pBD->addc_offset += 1;
            return pBD;
        }
    }
    if (pBD->mode == 0)
    {
        if (b_isfull(pBD) == 1)
        {
            return NULL;
        }
        else
        {
            pBD->flags &= RESET_R_FLAG;
            pBD->cb_head[pBD->addc_offset] = symbol;
            pBD->addc_offset += 1;
            return pBD;
        }
    }
    else
    {
        return NULL;
    }
}
int b_clear(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    else
    {
        pBD->markc_offset = 0;
        pBD->addc_offset = 0;
        pBD->getc_offset = 0;
        pBD->flags = DEFAULT_FLAGS;

        return 0;
    }
}
/*Purpose: Free the buffer
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: realloc, b_isfull
* Parameters: Buffer *
* Return value: VOID
* Algorithm: */
void b_free(Buffer *const pBD)
{
    free(pBD->cb_head);
    free(pBD);
}
/*Purpose: Check for full buffer
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: Buffer *
* Return value: int (-1 , 0, 1)
* Algorithm:*/
int b_isfull(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    if (pBD->addc_offset == pBD->capacity)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}
/*Purpose: Find addc_offset
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: Buffer *
* Return value: addc_offset, -1
* Algorithm: */
short b_limit(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    return pBD->addc_offset;
}
/*Purpose: get capacity
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: Buffer *
* Return value: pBD->capacity;, -1
* Algorithm: */
short b_capacity(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    return pBD->capacity;
}
/*Purpose: set mark
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: Buffer * short mark
* Return value: markc_offset, -1
* Algorithm: */
short b_mark(pBuffer const pBD, short mark)
{
    if (pBD == NULL)
    {
        return -1;
    }
    if (mark > pBD->addc_offset)
    {
        return -1;
    }
    pBD->markc_offset = mark;
    return pBD->markc_offset;
}
/*Purpose: get mode
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: Buffer *
* Return value: pBD->mode
* Algorithm: */
int b_mode(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    return pBD->mode;
}
/*Purpose: get inc_factor
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: Buffer *
* Return value: pBD->inc_factor, or 0x100
* Algorithm: */
size_t b_incfactor(Buffer *const pBD) /*Handle error 0x001*/
{
    if (pBD == NULL)
    {
        return 0x100;
    }
    return pBD->inc_factor;
}
/*Purpose: load file
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: b_addc, fgetc, feof (macro)
* Parameters: Buffer *, file *
* Return value: A count of chars, -1
* Algorithm: Load file until EOF, if file can't be loaded return a fail, increment counter for each char*/
int b_load(FILE *const fi, Buffer *const pBD)
{
    char new_char; /* holds the char */
    short char_counter; /* Counts the char */
    if (pBD == NULL)
    {
        return -1;
    }

    char_counter = 0;

    while (1) /*Remember to check this*/
    {
        new_char = fgetc(fi);
        if (feof(fi))
        {
            break;
        }
        if (b_addc(pBD, new_char) == NULL)
        {
            ungetc(new_char, fi);
            return LOAD_FAIL;
        }
        else
        {
            ++char_counter;
        }
    }
    return char_counter;
}
/*Purpose: Check if buffer is empty
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: b_addc, fgetc, feof (macro)
* Parameters: buffer *
* Return value: -1, 0, 1
* Algorithm: */
int b_isempty(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    if (pBD->addc_offset == 0)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}
/*Purpose: Read the buffer, and set EOB
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: buffer *
* Return value: 0, -2, or a char
* Algorithm: Read the buffer using offsets, if the offsets match mark the EOB*/
char b_getc(Buffer *const pBD)
{
    char r_char; /* The char to return */
    if (pBD == NULL)
    {
        return -2;
    }
    if (pBD->addc_offset == pBD->getc_offset)
    {
        pBD->flags |= SET_EOB;
        return 0;
    }
    else
    {
        pBD->flags &= RESET_EOB;
        r_char = pBD->cb_head[pBD->getc_offset];
        pBD->getc_offset += 1;
        return r_char;
    }
}
/*Purpose: Checks the EB
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: buffer *
* Return value: 0 or 2
* Algorithm: */
int b_eob(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    /*Maybe add some logic here for empty buffer*/
    return (CHECK_EOB & pBD->flags);
}
/*Purpose: Print buffer contents
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: b_getc, b_eob, printf
* Parameters: buffer *, char
* Return value: getc_offset, or -1
* Algorithm: Read the buffer until EOB is set, check if newline needs to be added*/
int b_print(Buffer *const pBD, char nl)
{
    char print_char;

    if (pBD == NULL)
    {
        return -1;
    }
    print_char = b_getc(pBD);
    while (!b_eob(pBD))
    {
        printf("%c", print_char);
        print_char = b_getc(pBD);
    }
    if (nl != 0)
    {
        printf("\n");
        return pBD->getc_offset;
    }
    else
    {
        return pBD->getc_offset;
    }
}
/*Purpose: Compact the buffer as to make it space efficient
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: realloc, b_limit
* Parameters: buffer *, char
* Return value: NULL, Buffer *
* Algorithm: check parameters, resize the buffer plus space for EOF, check pointer address, add EOF*/
Buffer *b_compact(Buffer *const pBD, char symbol)
{
    char *temp_array;
    short new_capacity;
    if (pBD == NULL)
    {
        return NULL;
    }
    if (pBD->capacity < 0)
    {
        printf("B_compact - Capacity is negative");
        return NULL;
    }
    new_capacity = b_limit(pBD) + 1;
    temp_array = (char *)realloc(pBD->cb_head, new_capacity);
    if (temp_array == NULL)
    {
        printf("Could not reallocate memory");
        return NULL;
    }
    else
    {
        if (temp_array != pBD->cb_head)
        {
            pBD->flags |= SET_R_FLAG;
        }
        pBD->capacity = new_capacity;
        pBD->cb_head = temp_array;
        pBD->cb_head[pBD->addc_offset] = symbol;
        pBD->addc_offset += 1;
        return pBD;
    }
}
/*Purpose: Check relocation of buffer
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: buffer *
* Return value: char
* Algorithm: */
char b_rflag(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }

    return (CHECK_R_FLAG & pBD->flags);
}
/*Purpose: move the offset back 1
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: short
* Return value: getc_offset;
* Algorithm: */
short b_retract(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }

    if (pBD->getc_offset <= 0) 
    {
        return -1;
    }
    else
    {
        pBD->getc_offset -= 1;
        return pBD->getc_offset;
    }
}
/*Purpose: Set the getc_offset to markc_offset
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: short
* Return value: getc_offset;
* Algorithm: */
short b_reset(Buffer *const pBD)
{
    if (pBD == NULL) 
    {
        return -1;
    }
    pBD->getc_offset = pBD->markc_offset;
    return pBD->getc_offset;
}
/*Purpose: get the getc_offset
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: short
* Return value: getc_offset;
* Algorithm: */
short b_getcoffset(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    return pBD->getc_offset;
}
/*Purpose: set the mark and getc_offset to 0
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: buffer *
* Return value: -1, 0
* Algorithm: */
int b_rewind(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return -1;
    }
    pBD->getc_offset = 0;
    pBD->markc_offset = 0;
    return 0;
}
/*Purpose: get a pointer to markc_offset
* Author: Johnathan Mangos
* History/Versions: 1.0 Oct 9th
* Called functions: 
* Parameters: buffer *
* Return value: char *
* Algorithm: */
char *b_location(Buffer *const pBD)
{
    if (pBD == NULL)
    {
        return NULL;
    }
    return (pBD->cb_head + pBD->markc_offset);
}