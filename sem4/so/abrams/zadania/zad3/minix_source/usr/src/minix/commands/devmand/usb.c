#ifndef lint
#if __GNUC__ - 0 >= 4 || (__GNUC__ - 0 == 3 && __GNUC_MINOR__ >= 1)
__attribute__((__used__))
#endif
static const char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif

#ifdef _LIBC
#include "namespace.h"
#endif
#include <stdlib.h>
#include <string.h>

#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9

#define YYEMPTY        (-1)
#define yyclearin      (yychar = YYEMPTY)
#define yyerrok        (yyerrflag = 0)
#define YYRECOVERING() (yyerrflag != 0)

#define YYPREFIX "yy"

#define YYPURE 0

#line 2 "usb.y"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "usb_driver.h"
#define YY_NO_INPUT
static struct devmand_usb_driver   *current_drv;
static struct devmand_usb_match_id *current_id;

int yylex(void);

void yyerror(char *s)
{
    fprintf(stderr,"parsing error: %s\n",s);
}

int yywrap()
{
    return 1;
}
#line 23 "usb.y"
#ifdef YYSTYPE
#undef  YYSTYPE_IS_DECLARED
#define YYSTYPE_IS_DECLARED 1
#endif
#ifndef YYSTYPE_IS_DECLARED
#define YYSTYPE_IS_DECLARED 1
typedef union {
       char *string;
} YYSTYPE;
#endif /* !YYSTYPE_IS_DECLARED */
#line 58 "y.tab.c"

/* compatibility with bison */
#ifdef YYPARSE_PARAM
/* compatibility with FreeBSD */
# ifdef YYPARSE_PARAM_TYPE
#  define YYPARSE_DECL() yyparse(YYPARSE_PARAM_TYPE YYPARSE_PARAM)
# else
#  define YYPARSE_DECL() yyparse(void *YYPARSE_PARAM)
# endif
#else
# define YYPARSE_DECL() yyparse(void)
#endif

/* Parameters sent to lex. */
#ifdef YYLEX_PARAM
# define YYLEX_DECL() yylex(void *YYLEX_PARAM)
# define YYLEX yylex(YYLEX_PARAM)
#else
# define YYLEX_DECL() yylex(void)
# define YYLEX yylex()
#endif

/* Parameters sent to yyerror. */
#ifndef YYERROR_DECL
#define YYERROR_DECL() yyerror(const char *s)
#endif
#ifndef YYERROR_CALL
#define YYERROR_CALL(msg) yyerror(msg)
#endif

extern int YYPARSE_DECL();


#define USB_DRIVER 257
#define DEV_PREFIX 258
#define BINARY 259
#define INTERFACE_CLASS 260
#define INTERFACE_SUB_CLASS 261
#define EQUALS 262
#define DEV_TYPE 263
#define BLOCK_DEV 264
#define CHAR_DEV 265
#define UPSCRIPT 266
#define DOWNSCRIPT 267
#define SEMICOLON 268
#define BRACKET_OPEN 269
#define BRACKET_CLOSE 270
#define STRING 271
#define ID 272
#define INTERFACE_PROTOCOL 273
#define YYERRCODE 256
static const short yylhs[] = {                           -1,
    0,    0,    2,    1,    3,    3,    5,    4,    4,    4,
    4,    4,    4,    4,    6,    6,    7,    7,    7,
};
static const short yylen[] = {                            2,
    1,    2,    0,    6,    1,    2,    0,    5,    4,    4,
    4,    4,    4,    4,    1,    2,    4,    4,    4,
};
static const short yydefred[] = {                         0,
    0,    0,    1,    3,    2,    0,    0,    0,    0,    0,
    0,    0,    0,    5,    0,    0,    0,    0,    0,    0,
    4,    6,    0,    0,    0,    0,    0,    0,    0,    0,
   10,    9,   11,   12,   13,   14,    0,    0,    0,    0,
   15,    0,    0,    0,    8,   16,    0,    0,    0,   17,
   18,   19,
};
static const short yydgoto[] = {                          2,
    3,    6,   13,   14,   15,   40,   41,
};
static const short yysindex[] = {                      -253,
 -264, -253,    0,    0,    0, -261, -241, -248, -238, -235,
 -234, -233, -247,    0, -263, -250, -240, -259, -239, -237,
    0,    0, -236, -232, -231, -230, -229, -228, -227, -258,
    0,    0,    0,    0,    0,    0, -220, -219, -218, -260,
    0, -226, -225, -224,    0,    0, -217, -216, -215,    0,
    0,    0,
};
static const short yyrindex[] = {                         0,
    0,    0,    0,    0,    0,    0, -242,    0,    0,    0,
    0,    0, -242,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,
};
static const short yygindex[] = {                         0,
   33,    0,    0,   35,    0,    0,    9,
};
#define YYTABLESIZE 53
static const short yytable[] = {                         37,
   38,   37,   38,    1,   26,   27,    4,    7,   23,   45,
    8,    9,   39,   16,   39,   10,    8,    9,   11,   12,
   24,   10,   21,   17,   11,   12,   18,   19,   20,    7,
   25,   28,   30,   29,    5,   31,   32,   33,   34,   35,
   36,   42,   43,   44,   47,   48,   49,   22,   46,    0,
   50,   51,   52,
};
static const short yycheck[] = {                        260,
  261,  260,  261,  257,  264,  265,  271,  269,  272,  270,
  258,  259,  273,  262,  273,  263,  258,  259,  266,  267,
  271,  263,  270,  262,  266,  267,  262,  262,  262,  272,
  271,  271,  269,  271,    2,  268,  268,  268,  268,  268,
  268,  262,  262,  262,  271,  271,  271,   13,   40,   -1,
  268,  268,  268,
};
#define YYFINAL 2
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 273
#if YYDEBUG
static const char *yyname[] = {

"end-of-file",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"USB_DRIVER","DEV_PREFIX",
"BINARY","INTERFACE_CLASS","INTERFACE_SUB_CLASS","EQUALS","DEV_TYPE",
"BLOCK_DEV","CHAR_DEV","UPSCRIPT","DOWNSCRIPT","SEMICOLON","BRACKET_OPEN",
"BRACKET_CLOSE","STRING","ID","INTERFACE_PROTOCOL",
};
static const char *yyrule[] = {
"$accept : drivers",
"drivers : driver",
"drivers : drivers driver",
"$$1 :",
"driver : USB_DRIVER STRING $$1 BRACKET_OPEN usb_driver_statements BRACKET_CLOSE",
"usb_driver_statements : usb_driver_statement",
"usb_driver_statements : usb_driver_statements usb_driver_statement",
"$$2 :",
"usb_driver_statement : $$2 ID BRACKET_OPEN usb_device_id_statements BRACKET_CLOSE",
"usb_driver_statement : BINARY EQUALS STRING SEMICOLON",
"usb_driver_statement : DEV_PREFIX EQUALS STRING SEMICOLON",
"usb_driver_statement : DEV_TYPE EQUALS BLOCK_DEV SEMICOLON",
"usb_driver_statement : DEV_TYPE EQUALS CHAR_DEV SEMICOLON",
"usb_driver_statement : UPSCRIPT EQUALS STRING SEMICOLON",
"usb_driver_statement : DOWNSCRIPT EQUALS STRING SEMICOLON",
"usb_device_id_statements : usb_device_id_statement",
"usb_device_id_statements : usb_device_id_statements usb_device_id_statement",
"usb_device_id_statement : INTERFACE_CLASS EQUALS STRING SEMICOLON",
"usb_device_id_statement : INTERFACE_SUB_CLASS EQUALS STRING SEMICOLON",
"usb_device_id_statement : INTERFACE_PROTOCOL EQUALS STRING SEMICOLON",

};
#endif

int      yydebug;
int      yynerrs;

int      yyerrflag;
int      yychar;
YYSTYPE  yyval;
YYSTYPE  yylval;

/* define the initial stack-sizes */
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH  YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH  500
#endif
#endif

#define YYINITSTACKSIZE 500

typedef struct {
    unsigned stacksize;
    short    *s_base;
    short    *s_mark;
    short    *s_last;
    YYSTYPE  *l_base;
    YYSTYPE  *l_mark;
} YYSTACKDATA;
/* variables for the parser stack */
static YYSTACKDATA yystack;

#if YYDEBUG
#include <stdio.h>		/* needed for printf */
#endif

#include <stdlib.h>	/* needed for malloc, etc */
#include <string.h>	/* needed for memset */

/* allocate initial stack or double stack size, up to YYMAXDEPTH */
static int yygrowstack(YYSTACKDATA *data)
{
    int i;
    unsigned newsize;
    short *newss;
    YYSTYPE *newvs;

    if ((newsize = data->stacksize) == 0)
        newsize = YYINITSTACKSIZE;
    else if (newsize >= YYMAXDEPTH)
        return -1;
    else if ((newsize *= 2) > YYMAXDEPTH)
        newsize = YYMAXDEPTH;

    i = (int) (data->s_mark - data->s_base);
    newss = (short *)realloc(data->s_base, newsize * sizeof(*newss));
    if (newss == 0)
        return -1;

    data->s_base = newss;
    data->s_mark = newss + i;

    newvs = (YYSTYPE *)realloc(data->l_base, newsize * sizeof(*newvs));
    if (newvs == 0)
        return -1;

    data->l_base = newvs;
    data->l_mark = newvs + i;

    data->stacksize = newsize;
    data->s_last = data->s_base + newsize - 1;
    return 0;
}

#if YYPURE || defined(YY_NO_LEAKS)
static void yyfreestack(YYSTACKDATA *data)
{
    free(data->s_base);
    free(data->l_base);
    memset(data, 0, sizeof(*data));
}
#else
#define yyfreestack(data) /* nothing */
#endif

#define YYABORT  goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR  goto yyerrlab

int
YYPARSE_DECL()
{
    int yym, yyn, yystate;
#if YYDEBUG
    const char *yys;

    if ((yys = getenv("YYDEBUG")) != 0)
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = YYEMPTY;
    yystate = 0;

#if YYPURE
    memset(&yystack, 0, sizeof(yystack));
#endif

    if (yystack.s_base == NULL && yygrowstack(&yystack)) goto yyoverflow;
    yystack.s_mark = yystack.s_base;
    yystack.l_mark = yystack.l_base;
    yystate = 0;
    *yystack.s_mark = 0;

yyloop:
    if ((yyn = yydefred[yystate]) != 0) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = YYLEX) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
        {
            goto yyoverflow;
        }
        yystate = yytable[yyn];
        *++yystack.s_mark = yytable[yyn];
        *++yystack.l_mark = yylval;
        yychar = YYEMPTY;
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;

    yyerror("syntax error");

    goto yyerrlab;

yyerrlab:
    ++yynerrs;

yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yystack.s_mark]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yystack.s_mark, yytable[yyn]);
#endif
                if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
                {
                    goto yyoverflow;
                }
                yystate = yytable[yyn];
                *++yystack.s_mark = yytable[yyn];
                *++yystack.l_mark = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yystack.s_mark);
#endif
                if (yystack.s_mark <= yystack.s_base) goto yyabort;
                --yystack.s_mark;
                --yystack.l_mark;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = YYEMPTY;
        goto yyloop;
    }

yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    if (yym)
        yyval = yystack.l_mark[1-yym];
    else
        memset(&yyval, 0, sizeof yyval);
    switch (yyn)
    {
case 1:
#line 34 "usb.y"
	{
	}
break;
case 2:
#line 37 "usb.y"
	{
	}
break;
case 3:
#line 41 "usb.y"
	{current_drv = add_usb_driver(yystack.l_mark[0].string);}
break;
case 4:
#line 44 "usb.y"
	{
	}
break;
case 5:
#line 49 "usb.y"
	{
	}
break;
case 6:
#line 52 "usb.y"
	{
	}
break;
case 7:
#line 56 "usb.y"
	{current_id = add_usb_match_id(current_drv);}
break;
case 8:
#line 58 "usb.y"
	{
	}
break;
case 9:
#line 61 "usb.y"
	{
		current_drv->binary = yystack.l_mark[-1].string;
	}
break;
case 10:
#line 65 "usb.y"
	{
		current_drv->devprefix = yystack.l_mark[-1].string;
	}
break;
case 11:
#line 69 "usb.y"
	{
		current_drv->dev_type = block_dev;
	}
break;
case 12:
#line 73 "usb.y"
	{
		current_drv->dev_type = char_dev;
	}
break;
case 13:
#line 77 "usb.y"
	{
		current_drv->upscript = yystack.l_mark[-1].string;
	}
break;
case 14:
#line 81 "usb.y"
	{
		current_drv->downscript = yystack.l_mark[-1].string;
	}
break;
case 15:
#line 88 "usb.y"
	{
	}
break;
case 16:
#line 91 "usb.y"
	{
	}
break;
case 17:
#line 97 "usb.y"
	{
		int res;
		unsigned int num;
		current_id->match_flags |= USB_MATCH_INTERFACE_CLASS;
		res =  sscanf(yystack.l_mark[-1].string, "0x%x", &num);
		if (res != 1) {
			fprintf(stderr, "ERROR");
			exit(1);
		}
		current_id->match_id.bInterfaceClass = num;
	}
break;
case 18:
#line 109 "usb.y"
	{
		int res;
		unsigned int num;
		current_id->match_flags |= USB_MATCH_INTERFACE_SUBCLASS;
		res =  sscanf(yystack.l_mark[-1].string, "0x%x", &num);
		if (res != 1) {
			fprintf(stderr, "ERROR");
			exit(1);
		}
		current_id->match_id.bInterfaceSubClass = num;

	}
break;
case 19:
#line 122 "usb.y"
	{
		int res;
		unsigned int num;
		current_id->match_flags |= USB_MATCH_INTERFACE_PROTOCOL;
		res =  sscanf(yystack.l_mark[-1].string, "0x%x", &num);
		if (res != 1) {
			fprintf(stderr, "ERROR");
			exit(1);
		}
		current_id->match_id.bInterfaceProtocol = num;

	}
break;
#line 575 "y.tab.c"
    }
    yystack.s_mark -= yym;
    yystate = *yystack.s_mark;
    yystack.l_mark -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yystack.s_mark = YYFINAL;
        *++yystack.l_mark = yyval;
        if (yychar < 0)
        {
            if ((yychar = YYLEX) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yystack.s_mark, yystate);
#endif
    if (yystack.s_mark >= yystack.s_last && yygrowstack(&yystack))
    {
        goto yyoverflow;
    }
    *++yystack.s_mark = (short) yystate;
    *++yystack.l_mark = yyval;
    goto yyloop;

yyoverflow:
    yyerror("yacc stack overflow");

yyabort:
    yyfreestack(&yystack);
    return (1);

yyaccept:
    yyfreestack(&yystack);
    return (0);
}
