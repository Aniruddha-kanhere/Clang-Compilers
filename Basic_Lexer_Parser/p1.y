	//Aniruddha Kanhere        200261603          arkanher@ncsu.edu
	
%{
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>

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
#include "llvm/IR/DerivedTypes.h"

#include "llvm-c/Core.h"

#define DEBUG 0                       //0: Disable, 1: Enable -> printf based debugging;

using namespace llvm;

extern FILE *yyin;
int yylex(void);
int yyerror(const char *);

extern char *fileNameOut;

extern Module *M;
extern LLVMContext TheContext;

// IRBuilder Class object, initialize it later //
IRBuilder<> Builder(TheContext);

// Variables used throughout the program //
int params_cnt=0;
llvm::Value *my_val;
Function *Func;
std::vector<Type*> params;
std::map<char *, llvm::Value *> my_map;
IRBuilder<> Builder(TheContext);

int params_cnt=0;
%}

%union {
  int num;
  char *id;
  llvm::Value *val;
}

%token DEF ASSIGN SEMI COMMA MINUS PLUS LBRACKET RBRACKET SUM TMP NUM ID RAISE HASH LESSTHAN
%type <num> NUM
%type <val> expr
}

%token DEF ASSIGN SEMI COMMA MINUS PLUS LBRACKET RBRACKET SUM TMP NUM ID RAISE HASH 
%type <num> NUM 
%type <id> ID


//%nonassoc QUESTION COLON
%left PLUS MINUS
%left MULTIPLY DIVIDE RAISE
%left LESSTHAN

%left MULTIPLY DIVIDE
%left RAISE

%start program

%%

program: define stmtlist 
{ 
  Builder.CreateRet(my_val);
  /* 
    IMPLEMENT: return value
    Hint: the following code is not sufficient
  */    
  Builder.CreateRet(Builder.getInt64(0));
  return 0;
}
;

define: DEF ID LBRACKET NUM RBRACKET SEMI 
{
  params_cnt = $4;
  
  std::vector<Type*> params;

  for(int i=0; i<params_cnt; i++)
    params.push_back(Builder.getInt64Ty());

  FunctionType *FunType = FunctionType::get(Builder.getInt64Ty(),params,false);

  // Create a main function
  Func = Function::Create(FunType,GlobalValue::ExternalLinkage,$2,M);
  Function *Func = Function::Create(FunType,GlobalValue::ExternalLinkage,$2,M);
  
  //Add a basic block to the function
  BasicBlock *BB = BasicBlock::Create(TheContext,"entry",Func);

  Builder.SetInsertPoint(BB);  
}
;

stmtlist:  stmtlist stmt 
         | stmt                   
;         


stmt: ID ASSIGN expr SEMI
{
    my_val = $3;         /* Assign the value of expression to a variable each time this rule is matched.
                            This variable is then returned later.                                          */

    my_map[$1] = $3;     /* Store each new ID that we come across along with its value                     */
    #if DEBUG
    printf("New %s = %ld\n", $1, (cast<ConstantInt>($3))->getZExtValue());
    #endif
}
;

expr:   expr MINUS expr
	{
          #if DEBUG
  	  printf("expr Minus expr %ld - %ld\n", (cast<ConstantInt>($1))->getSExtValue(), (cast<ConstantInt>($3))->getSExtValue());   
	  #endif
	  $$ = Builder.CreateSub($1,$3,"");
	}
      | expr PLUS expr
	{
          #if DEBUG
	  printf("expr PLUS expr %ld + %ld\n", (cast<ConstantInt>($1))->getSExtValue(), (cast<ConstantInt>($3))->getSExtValue());
	  #endif
	  $$ = Builder.CreateAdd($1,$3,"");
	}
      | expr LESSTHAN expr
        {
          Value *res = Builder.CreateICmpSLT($1,$3);
          $$ = Builder.CreateSelect(res, llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext), 1), llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext), 0));
        }
      | expr RAISE expr
	{
          #if DEBUG
	  printf("expr RAISE expr %ld ^^ %ld\n", (cast<ConstantInt>($1))->getSExtValue(), (cast<ConstantInt>($3))->getSExtValue());
	  #endif
          
          // Check if the $3 value is a constant or not. In case it isn't we shall generate an Error message
          if(isa<ConstantInt>($3))
          {
             Value *v;

             //  If the value of $3 is negative, we shall simply return 0 (Since all data is  //
             //  of int type and doesn't support floating point opeartions                    //
             if((cast<ConstantInt>($3))->getSExtValue()  < 0)
             {
                v = llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext), 0);
             }
             else
             {
                //  Since the value is a constant, we can loop through it to essentially      //
                //  implement the exponentiation operator                                     //
		v = llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext), 1);
     		for(int i=0;i<(cast<ConstantInt>($3))->getSExtValue(); i++)
		{
			v = Builder.CreateMul(v,$1,"");
		}
	     }
             $$ = v;		
          }
          else
          {
		printf("Error!, exponent not a constant! Exiting...\n");	        
		return -1;
          }	 
	}
      | MINUS expr
	{
          #if DEBUG
	  printf("MINUS expr - %ld\n", (cast<ConstantInt>($2))->getSExtValue());
	  #endif
	  $$ = Builder.CreateNeg($2,"");
	}
      | expr MULTIPLY expr
	{
	  $$ = Builder.CreateMul($1,$3,"");

          #if DEBUG
	  printf("expr MULTIPLY expr %ld * %ld\n", (cast<ConstantInt>($1))->getSExtValue(), (cast<ConstantInt>($3))->getSExtValue());
	  #endif
	}
      | expr DIVIDE expr
	{	
	  $$ = Builder.CreateSDiv($1,$3,"");

          #if DEBUG	
	  printf("expr DIVIDE expr %ld / %ld\n", (cast<ConstantInt>($1))->getSExtValue(), (cast<ConstantInt>($3))->getSExtValue());
	  #endif
	}
      | LBRACKET expr RBRACKET
	{
          if(isa<ConstantInt>$2)
       	  {
	          #if DEBUG
		  printf("Constant Value\n");	
	   	  #endif
                  if(((cast<ConstantInt>($2))->getSExtValue() < 0)  || ((cast<ConstantInt>($2))->getSExtValue() >= (long)params_cnt))
                     $$ = llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext),0);
                  else
  		     $$ = (Value *)(&(Func->arg_begin()[(dyn_cast<ConstantInt>($2))->getZExtValue()]));
          }
          else
          {
	          #if DEBUG
		  printf("NOT Constant Value\n");
	  	  #endif
                  Value *v = llvm::ConstantInt::get(Builder.getInt64Ty(),0);
        	  for(int i=0; i<params_cnt; i++)
                  {
 		 	Value *res = Builder.CreateICmpEQ($2, llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext),i));
                        v = Builder.CreateSelect(res, (Value *)(&(Func->arg_begin()[i])), v);
		  }                               
                  $$ = v;
	  }
	}  
      | ID
	{
	  std::map<char *, llvm::Value *>::iterator it;
          for(it = my_map.begin(); it!=my_map.end();it++)
	  {
             if(!strcmp($1, it->first))
             {                
                $$ = it->second;
	        #if DEBUG
 		printf("Found %s! Value: %ld\n", $1, (cast<ConstantInt>($$))->getSExtValue());
	        #endif
                break;
             }
	  }
	}
      | NUM
	{
   	  $$ = llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext), $1);
          #if DEBUG
	  printf("NUM %d\n", $1);
	  #endif	  
	}
      | HASH
	{	  
	  $$ = llvm::ConstantInt::get(llvm::Type::getInt64Ty(TheContext), params_cnt);
          #if DEBUG
          printf("HASH %lu\n",(cast<ConstantInt>($$))->getZExtValue());
	  #endif
	}
  Builder.SetInsertPoint(BB);
}
;


%%


void initialize()
{
  /* add something here if needed */
}

extern int line;

int yyerror(const char *msg)
{
  printf("%s at line %d.\n",msg,line);
  return 0;
}
