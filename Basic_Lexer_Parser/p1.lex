	//Aniruddha Kanhere        200261603          arkanher@ncsu.edu
	

%{ 
/* P1. Implements scanner*/

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/FileSystem.h"

typedef struct myvalue {
  int size;
  LLVMValueRef val[32];
} MyValue;

int line=1;
void yyerr(const char *);

#include "p1.y.hpp" 
%}

%option nounput
%option noinput
 
%% 

\n           line++;
[\t ]        ;

def            { return DEF; }

"#"            { return HASH; }

[a-zA-Z_][_a-zA-Z0-9]*  { yylval.id = strdup(yytext); return ID; } 

[0-9]+          { yylval.num = atoi(yytext); return NUM; }

"="	{ return ASSIGN;      } 
";"	{ return SEMI;        } 
"-"	{ return MINUS;       } 
"+"	{ return PLUS;        }  
"*"	{ return MULTIPLY;    } 
"/"	{ return DIVIDE;      } 
","     { return COMMA;       }
"["     { return LBRACKET;    }
"]"     { return RBRACKET;    }
"^^"    { return RAISE;       }
"<"     { return LESSTHAN;    }
.       { yyerr(strdup(yytext));}

%%


void yyerr(const char *msg)
{
	printf("%s at line %d.\n",msg,line);
	return;
}

%%
