    //Aniruddha Kanhere 200261603 arkanher@ncsu.edu

%{
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"


#include <memory>
#include <algorithm>

#include "list.h"
#include "symbol.h"

#define Loop_kind 1
#define Switch_case_kind 2

using namespace llvm;

int num_errors;

int last_used_kind = 0, last_used_kind_save = 0;
int Broken = 1;

extern int yylex();   /* lexical analyzer generated from lex.l */

int yyerror(const char *error);
int parser_error(const char*);

void minic_abort();
char *get_filename();
int get_lineno();

int loops_found=0;

extern Module *M;

Function *Fun;
IRBuilder<> *Builder;

Value* BuildFunction(Type* RetType, const char *name, 
			   paramlist_t *params);

%}

/* Data structure for tree nodes*/

%union {
  int inum;
  int fnum;
  char * id;
  Type*  type;
  Value* value;
  BasicBlock *bb;
  paramlist_t *params;
}

/* these tokens are simply their corresponding int values, more terminals*/
%token SEMICOLON COMMA COLON
%token LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
%token ASSIGN PLUS MINUS STAR DIV MOD 
%token LT GT LTE GTE EQ NEQ NOT
%token LOGICAL_AND LOGICAL_OR
%token BITWISE_OR BITWISE_XOR LSHIFT RSHIFT BITWISE_INVERT

%token DOT ARROW AMPERSAND QUESTION_MARK

%token FOR WHILE IF ELSE DO STRUCT SIZEOF RETURN SWITCH
%token BREAK CONTINUE CASE
%token INT VOID FLOAT

/* no meaning, just placeholders */
%token STATIC AUTO EXTERN TYPEDEF CONST VOLATILE ENUM UNION REGISTER

/* NUMBER and ID have values associated with them returned from lex*/
%token <inum> CONSTANT_INTEGER /*data type of NUMBER is num union*/
%token <fnum> CONSTANT_FLOAT /*data type of NUMBER is num union*/
%token <id>  ID

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

/* values created by parser*/
%type <id> declarator
%type <params> param_list param_list_opt
%type <value> expression expr_opt
%type <value> assignment_expression
%type <value> conditional_expression
%type <value> constant_expression
%type <value> logical_OR_expression
%type <value> logical_AND_expression
%type <value> inclusive_OR_expression
%type <value> exclusive_OR_expression
%type <value> AND_expression
%type <value> equality_expression
%type <value> relational_expression
%type <value> shift_expression
%type <value> additive_expression
%type <value> multiplicative_expression
%type <value> cast_expression
%type <value> unary_expression
%type <value> lhs_expression
%type <value> postfix_expression
%type <value> primary_expression
%type <value> constant
%type <type> type_specifier
%type <value> opt_initializer

%%

translation_unit:	external_declaration
			| translation_unit external_declaration
;

external_declaration:	function_definition
			{
			  /* finish compiling function */
			  if(num_errors>100)
			  {
			    minic_abort();
			  }
			  else if(num_errors==0)
			  {    }
			}
                        | declaration 
			{ 
			  /* nothing to be done here */
			}
;

function_definition:	/* <type> <func_name> (p0, p1, ....) */
                        type_specifier ID LPAREN param_list_opt RPAREN 
			{
			  symbol_push_scope();
			  BuildFunction($1,$2,$4);  
			} 
                        compound_stmt 
			{ 
			  BasicBlock *BB = Builder->GetInsertBlock();

			  if(!BB->getTerminator())
			  { 
                            /* If no terminator, then make one in accordance with the type of function */
			    if(Fun->getFunctionType()->getReturnType()->isVoidTy())
				Builder->CreateRetVoid();
			    else if(Fun->getFunctionType()->getReturnType()->isFloatTy())
				Builder->CreateRet(ConstantFP::get(Builder->getFloatTy(),0.0));
			    else if(Fun->getFunctionType()->getReturnType()->isIntegerTy())
			    {
			      if(Fun->getFunctionType()->getReturnType() == Builder->getInt32Ty())
				  Builder->CreateRet(Builder->getInt32(0));
			      else if(Fun->getFunctionType()->getReturnType() == Builder->getInt1Ty())
				  Builder->CreateRet(Builder->getInt1(0));
			    }
			    else if(Fun->getFunctionType()->getReturnType()->isPointerTy())			    
			       Builder->CreateRet( ConstantPointerNull::get( PointerType::get(Fun->getFunctionType()->getReturnType(),0) ) );			    
                            else
  			       Builder->CreateRet(Builder->getInt32(0));
			  }
			  symbol_pop_scope();
			}

			/* <type *> <func_name> (p0, p1, ....) */
                        | type_specifier STAR ID LPAREN param_list_opt RPAREN 
			{
			  symbol_push_scope();
			  BuildFunction(PointerType::get($1,0),$3,$5);
			} 
                        compound_stmt 
			{			 
			  BasicBlock *BB = Builder->GetInsertBlock();

			  if(!BB->getTerminator())
			  {
			    /* If no terminator, then make one in accordance with the type of function */
			    if(Fun->getFunctionType()->getReturnType()->isVoidTy())
				Builder->CreateRetVoid();
			    else if(Fun->getFunctionType()->getReturnType()->isFloatTy())
				Builder->CreateRet(ConstantFP::get(Builder->getFloatTy(),0.0));
			    else if(Fun->getFunctionType()->getReturnType()->isIntegerTy())
			    {
			      if(Fun->getFunctionType()->getReturnType() == Builder->getInt32Ty())
				  Builder->CreateRet(Builder->getInt32(0));
			      else if(Fun->getFunctionType()->getReturnType() == Builder->getInt1Ty())
				  Builder->CreateRet(Builder->getInt1(0));
			    }
			    else if(Fun->getFunctionType()->getReturnType()->isPointerTy())
			    {
			       Builder->CreateRet( ConstantPointerNull::get( PointerType::get(Fun->getFunctionType()->getReturnType(),0) ) );
			    }
                            else
  			       Builder->CreateRet(Builder->getInt32(0));
			  }
			  symbol_pop_scope();
			}
;

declaration:    	type_specifier STAR ID opt_initializer SEMICOLON
			{
			    if (is_global_scope())
			    {
			      Twine name($3);
			      new GlobalVariable(*M,(Type*)PointerType::get($1,0),false,GlobalValue::ExternalLinkage,(Constant*)NULL,name);
			    } 
			    else
			    {
			      symbol_insert($3,  /* map name to alloca */
					    Builder->CreateAlloca(PointerType::get($1,0),NULL,$3)); /* build alloca */
			    }
			} 
             		| type_specifier ID opt_initializer SEMICOLON
			{
			  if (is_global_scope())
			    {
			      Twine name($2);
			      new GlobalVariable(*M,(Type*)$1,false,GlobalValue::ExternalLinkage,(Constant*)NULL,name);
			    }
			  else
			    {
			      symbol_insert($2,  /* map name to alloca */
					    Builder->CreateAlloca($1,NULL,$2)); /* build alloca */
			      if($3!=NULL)
				 Builder->CreateStore($3, symbol_find($2));
			    }
			} 
;

declaration_list:	declaration
			{ /*Do Nothing*/  }
                        | declaration_list declaration  
			{ /*Do Nothing*/  }
;


type_specifier:		INT 
			{
			    $$ = IntegerType::get(M->getContext(),32);
			}
			| FLOAT
			{
			    $$ = llvm::Type::getFloatTy(M->getContext());
			}
                        | VOID
			{
			    $$ = llvm::Type::getVoidTy(M->getContext());
			}
;


declarator:		ID
			{
			  $$ = $1;
			}
;

opt_initializer: 	ASSIGN constant_expression	      
			{
			  $$ = $2;
			}
			| // nothing
			{
			  // indicate there is none
			  $$ = NULL;
			}
;


param_list_opt:         //nothing  
			{
			  $$ = NULL;
			}
			| param_list
			{
			  $$ = $1;
			}
;

param_list:		param_list COMMA type_specifier declarator
			{
			  $$ = push_param($1,$4,$3);
			}
			| param_list COMMA type_specifier STAR declarator
			{
			  $$ = push_param($1,$5,(Type*)PointerType::get($3,0));
			}
                        | param_list COMMA type_specifier
			{
			  $$ = push_param($1,NULL,$3);
			}
			|  type_specifier declarator
			{
			  /* create a parameter list with this as the first entry */
			  $$ = push_param(NULL, $2, $1);
			}
			| type_specifier STAR declarator
			{
			  /* create a parameter list with this as the first entry */
			  $$ = push_param(NULL, $3, (Type*)PointerType::get($1,0));
			}
                        | type_specifier
			{
			  /* create a parameter list with this as the first entry */
			  $$ = push_param(NULL, NULL, $1);
			}
;

/* Types of statements */
statement:		  expr_stmt            
			| compound_stmt        
			| selection_stmt       
			| iteration_stmt       
			| jump_stmt            
                        | break_stmt
                        | continue_stmt
                        | case_stmt
;

expr_stmt:	        SEMICOLON            
			{ /*Do Nothing for this*/ }
			|  expression SEMICOLON       
			{ /*Do Nothing for this*/ }
;

compound_stmt:		LBRACE declaration_list_opt statement_list_opt RBRACE 
			{ /*Do Nothing for this*/ }
;

declaration_list_opt:	//Nothing
			{ /*Do Nothing for this*/ }
			| declaration_list
			{ /*Do Nothing for this*/ }
;

statement_list_opt:	//Nothing
			{ /*Do Nothing for this*/ }
			| statement_list
			{ /*Do Nothing for this*/ }
;

statement_list:		statement
			{ /*Do Nothing for this*/ }
			| statement_list statement
			{ /*Do Nothing for this*/ }
;

break_stmt:             BREAK SEMICOLON
			{
			   /* If break was used in loop, then do... */
			   if(last_used_kind == Loop_kind)
			   {
			      loop_info_t info = get_loop();

			      if(info.exit)
				Builder->CreateBr(info.exit);
			      else
				printf("Loop exit is null\n");
			    }

			   /* If break was used in switch, then do... */
			    else if(last_used_kind == Switch_case_kind)
			    {			      
			      switch_info_t info=get_switch();
			      if( info.default_case!=NULL )
			      {
				pop_switch();
				push_switch(info.default_case, info.switch_body, info.last_case, 1);
				Builder->CreateBr(info.default_case);
			      }       
			    }

			   /* If break was neither in switch nor in loop, then generate an error */
			    else
			    {
			       parser_error("Break not inside a switch or loop\n"); 
			    }   
			}
;

case_stmt:              CASE constant_expression COLON
			{
			  switch_info_t info = get_switch();
			  if( (info.default_case!=NULL)  && (info.broken == 1) )
			  {  			     
			     pop_switch();
			     BasicBlock *case_block = BasicBlock::Create(M->getContext(), "case", Fun);
			     push_switch(info.default_case, info.switch_body, case_block, 0);
			     info.switch_body->addCase((dyn_cast<ConstantInt>)$2, case_block);
			     Builder->SetInsertPoint(case_block);
			  }
			  else if( (info.default_case!=NULL)  && (info.broken != 1) )
			  {			     
			     pop_switch();

			     BasicBlock *case_block = BasicBlock::Create(M->getContext(), "case", Fun);
			     Builder->CreateBr(case_block);
			     
			     push_switch(info.default_case, info.switch_body, case_block, 0);
			     info.switch_body->addCase((dyn_cast<ConstantInt>)$2, case_block);
			     
			     Builder->SetInsertPoint(case_block);
			  }
			}
;

continue_stmt:          CONTINUE SEMICOLON
			{
			   loop_info_t info = get_loop();
			   if(info.exit)
				Builder->CreateBr(info.expr);
			   else
				parser_error("Continue not inside a loop\n");
				
			}
;

selection_stmt:		IF LPAREN expression RPAREN
			{
			   BasicBlock *then = BasicBlock::Create(M->getContext(), "if.then", Fun);
			   BasicBlock *els =  BasicBlock::Create(M->getContext(), "if.else", Fun);
			   BasicBlock *join = BasicBlock::Create(M->getContext(), "if.join", Fun);

			   if($3->getType() == Builder->getInt1Ty())
			   {
				Builder->CreateCondBr($3, then, els);			
			   }
			   else if($3->getType() == Builder->getInt32Ty())
			   {
				Builder->CreateCondBr(Builder->CreateICmpNE($3, Builder->getInt32(0)), then, els);
			   }
			   else if($3->getType() == Builder->getFloatTy())
			   {				
				Builder->CreateCondBr(Builder->CreateFCmpONE($3, ConstantFP::get(Builder->getFloatTy(),0.0)), then, els);
			   }
                           else if(PointerType *py = dyn_cast<PointerType>($3->getType()))
                           {
                                Builder->CreateCondBr(Builder->CreateICmpNE($3, ConstantPointerNull::get(py)),then,els);
                           }

			   push_if(then, els, join);

			   Builder->SetInsertPoint(then);
			}
			statement
			{
			  if_info_t info = get_if();
			  if(info.join == NULL)
			  {
			    parser_error("Loop_info_t info is not initialized\n");
			  }
                          if(!Builder->GetInsertBlock()->getTerminator())
			      Builder->CreateBr(info.join);    //to join block
			  Builder->SetInsertPoint(info.els);  //next thing is the else block
			}
			ELSE statement 
			{
			  if_info_t info = get_if();
			  pop_if();
                          if(!Builder->GetInsertBlock()->getTerminator())
			       Builder->CreateBr(info.join);    //to join block
			  Builder->SetInsertPoint(info.join);   //next thing is the join block
			}
			| SWITCH LPAREN expression RPAREN
			{
			  BasicBlock *default_case = BasicBlock::Create(M->getContext(), "Switch.Default", Fun);
			  SwitchInst *my_switch = Builder->CreateSwitch($3, default_case, 10);
			  push_switch(default_case, my_switch, NULL, 1);

			  last_used_kind_save = last_used_kind;
			  last_used_kind = Switch_case_kind;
			}
			statement 
			{
			  switch_info_t info = get_switch();
			  pop_switch();

			  last_used_kind = last_used_kind_save;

			  BasicBlock *bb = Builder->GetInsertBlock();
			  if(!bb->getTerminator())
			     Builder->CreateBr(info.default_case);

			  Builder->SetInsertPoint(info.default_case);
			}
;


iteration_stmt:		WHILE 
				{ 
				  BasicBlock *expr = BasicBlock::Create(M->getContext(), "while.expr", Fun);
				  Builder->CreateBr(expr);
				  Builder->SetInsertPoint(expr);

				  push_loop(expr,expr,expr,expr);

				} LPAREN expression RPAREN { 
				  // create new body and exit blocks
				  loop_info_t info = get_loop();
				  pop_loop();  
				  
				  BasicBlock *body = BasicBlock::Create(M->getContext(), "while.body", Fun); 
				  BasicBlock *exit = BasicBlock::Create(M->getContext(), "while.exit", Fun); 

				  // call push loop to record this loop's important blocks
				  push_loop(info.expr, body, body, exit);
 				  last_used_kind_save = last_used_kind;
				  last_used_kind = Loop_kind;
				  Builder->CreateCondBr($4, body,exit);
				  Builder->SetInsertPoint(body);  
				} statement {

				  loop_info_t info = get_loop();

				  // insert back edge from body to header
				  Builder->CreateBr(info.expr);
				  Builder->SetInsertPoint(info.exit);
				  
				  pop_loop();
 				  last_used_kind = last_used_kind_save;
				}

			| FOR LPAREN expr_opt SEMICOLON
				{
				  BasicBlock *expr = BasicBlock::Create(M->getContext(), "for.expr", Fun);
				  Builder->CreateBr(expr);
				  Builder->SetInsertPoint(expr);

				  push_loop(expr,NULL,NULL,NULL);
				}
				 expr_opt SEMICOLON expr_opt RPAREN
				{
				  loop_info_t info = get_loop();
				  pop_loop();

				  BasicBlock *body = BasicBlock::Create(M->getContext(), "for.body", Fun); 
				  BasicBlock *exit = BasicBlock::Create(M->getContext(), "for.exit", Fun); 

				  // Call push loop to record this loop's important blocks
				  push_loop(info.expr, body, body, exit);

 				  last_used_kind_save = last_used_kind;
				  last_used_kind = Loop_kind;

				  Builder->CreateCondBr($6, body,exit);
				  Builder->SetInsertPoint(body);  
				}
				statement
				{
				  loop_info_t info = get_loop();
				  
				  // insert back edge from body to header
				  Builder->CreateBr(info.expr);
				  Builder->SetInsertPoint(info.exit);
				  
				  pop_loop();  
 				  last_used_kind = last_used_kind_save;
				}
			| DO 
				{
				   BasicBlock *body = BasicBlock::Create(M->getContext(), "DoWhile.body", Fun);
				   BasicBlock *expr = BasicBlock::Create(M->getContext(), "DoWhile.expr", Fun);
				   BasicBlock *exit = BasicBlock::Create(M->getContext(), "DoWhile.exit", Fun);

				   push_loop(expr, body, NULL, exit);
				   Builder->CreateBr(body);
				   Builder->SetInsertPoint(body);
 				   last_used_kind_save = last_used_kind;
				   last_used_kind = Loop_kind;
				}
				statement WHILE
				{
				   loop_info_t my_loop = get_loop();
				   Builder->CreateBr(my_loop.expr);
				   Builder->SetInsertPoint(my_loop.expr);
				}
				LPAREN expression RPAREN SEMICOLON
				{
				   loop_info_t my_loop = get_loop();
				   pop_loop();
 				   last_used_kind = last_used_kind_save;
				   Builder->CreateCondBr($7, my_loop.body, my_loop.exit);
				   Builder->SetInsertPoint(my_loop.exit);
				}
;

expr_opt:		
			{ 
			   $$ = NULL;
			}
			| expression
			{ 
			   $$ = $1;
			}
;

jump_stmt:		RETURN SEMICOLON
			{ 
			   Builder->CreateRetVoid();
			}
			| RETURN expression SEMICOLON
			{
                          if(Fun->getFunctionType()->getReturnType()  != $2->getType())
                          {
				if($2->getType() == Builder->getInt32Ty())
				{
				  if(Fun->getFunctionType()->getReturnType() == Builder->getFloatTy())
				  {
				     Value *val = new SIToFPInst($2, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				     Builder->CreateRet(val);
				  }
				  else if(Fun->getFunctionType()->getReturnType() == Builder->getInt1Ty())
				  {
				     Value *val = Builder->CreateICmpNE($2,Builder->getInt32(0));
				     Builder->CreateRet(val);
				  }
				}
				else if($2->getType() == Builder->getFloatTy())
				{
				  if(Fun->getFunctionType()->getReturnType() == Builder->getInt32Ty())
				  {
				     Value *val = new FPToSIInst($2, Builder->getInt32Ty(), "", Builder->GetInsertBlock());
				     Builder->CreateRet(val);
				  }
				  else if(Fun->getFunctionType()->getReturnType() == Builder->getInt1Ty())
				  {
				     Value *val = new FPToSIInst($2, Builder->getInt32Ty(), "", Builder->GetInsertBlock());
				     Value *val1 = Builder->CreateICmpNE(val,Builder->getInt32(0));
				     Builder->CreateRet(val1);
				  }
				}
				else if($2->getType() == Builder->getInt1Ty())
				{
				  if(Fun->getFunctionType()->getReturnType() == Builder->getInt32Ty())
				  {
				     Value *val = Builder->CreateZExt($2,Builder->getInt32Ty());
				     Builder->CreateRet(val);
				  }
				  else if(Fun->getFunctionType()->getReturnType() == Builder->getFloatTy())
				  {
				     Value *val = Builder->CreateZExt($2,Builder->getInt32Ty());
				     Value *val1 = new SIToFPInst(val, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				     Builder->CreateRet(val1);
				  }
				}
                                else
                                   parser_error("Return type doesn't match with the returned value\n");
			  }
			  else                          
				Builder->CreateRet($2);
			}
;

expression:             assignment_expression
			{ 
			  $$=$1;
			}
;

assignment_expression:  conditional_expression
			{
			  $$=$1;
			}
                        | lhs_expression ASSIGN assignment_expression
			{                          
                          if( $1->getType()->getPointerElementType() != $3->getType() )
                          {				
				  if($1->getType()->getPointerElementType() == Builder->getInt32Ty())
		                  {				     
	 			     if($3->getType() == Builder->getFloatTy())
				     {
					Value *v = new FPToSIInst($3, Builder->getInt32Ty(), "", Builder->GetInsertBlock());
		                     	Builder->CreateStore(v,$1);
				     }
	 			     else if($3->getType() == Builder->getInt1Ty())
				     {
		                     	Builder->CreateStore(Builder->CreateZExt($3, Builder->getInt32Ty()),$1);
				     }
				     else
				     {
		                     	Builder->CreateStore($3,$1);
				     }
		                  }
				  else if($1->getType()->getPointerElementType() == Builder->getFloatTy())
		                  {
	 			     if($3->getType() == Builder->getInt32Ty())
				     {
					Value *v = new SIToFPInst($3, Builder->getInt32Ty(), "", Builder->GetInsertBlock());
		                     	Builder->CreateStore(v,$1);
				     }
	 			     else if($3->getType() == Builder->getInt1Ty())
				     {
		                        Value *v = new SIToFPInst( Builder->CreateZExt($3, Builder->getInt32Ty()) , Builder->getInt32Ty(), "", Builder->GetInsertBlock());
		                     	Builder->CreateStore(v,$1);
				     }
				     else
				     {
					Builder->CreateStore($3,$1);
				     }
		                  }
                                  else
					Builder->CreateStore($3,$1);
			  }
			  else
				Builder->CreateStore($3,$1);                           			  
			}			  
;

constant_expression:    conditional_expression
			{ $$ = $1; }
;

conditional_expression: logical_OR_expression
			{
			  $$=$1;
			}
                        | logical_OR_expression QUESTION_MARK expression COLON conditional_expression
			{
			  Value *val=nullptr;
			  if(Builder->getFloatTy() == $1->getType())  
				val = Builder->CreateFCmpONE($1,ConstantFP::get(Builder->getFloatTy(),0.0));	
			  else if(Builder->getInt32Ty() == $1->getType())
				val = Builder->CreateICmpNE($1, Builder->getInt32(0));

			  $$ = Builder->CreateSelect(val, $3, $5);
			}
;

logical_OR_expression:  logical_AND_expression
			{
			  $$ = $1;
			}
                        | logical_OR_expression LOGICAL_OR
			{  
			  BasicBlock *first = BasicBlock::Create(M->getContext(), "SS.Check", Fun);
			  BasicBlock *exit =  BasicBlock::Create(M->getContext(), "SS.exit", Fun);

			  Value *val = nullptr;
			  if($1->getType() == Builder->getFloatTy())
			     val = Builder->CreateFCmpONE($1, ConstantFP::get(Builder->getFloatTy(),0.0));
			  else if($1->getType() == Builder->getInt32Ty())
			     val= Builder->CreateICmpNE($1, Builder->getInt32(0));
			  else if($1->getType() == Builder->getInt1Ty())
			     val = $1;
                          else if(PointerType *py = dyn_cast<PointerType>($1->getType()) )                          
                             val = Builder->CreateICmpNE($1, ConstantPointerNull::get(py));                          
                          else
                             parser_error("Unknown operand in logical_OR\n");
			    
			  Builder->CreateCondBr(val,exit,first);
			  push_loop(first, exit, NULL, Builder->GetInsertBlock());
			  Builder->SetInsertPoint(first);
			}
			logical_AND_expression
			{
			  Value *val1=nullptr;

			  if($4->getType() == Builder->getFloatTy())
			     val1 = Builder->CreateFCmpONE($4, ConstantFP::get(Builder->getFloatTy(),0.0));
			  else if($4->getType() == Builder->getInt32Ty())
			     val1 = Builder->CreateICmpNE($4, Builder->getInt32(0));
			  else if($4->getType() == Builder->getInt1Ty())
			     val1 = $4;
			  else if(PointerType *py = dyn_cast<PointerType>($4->getType()) )                          
                             val1 = Builder->CreateICmpNE($1, ConstantPointerNull::get(py));                          
                          else
			     parser_error("Unknown type of operand in Logical_OR\n");

			  loop_info_t info = get_loop();
			  pop_loop();
			  Builder->CreateBr(info.body);
			  Builder->SetInsertPoint(info.body); 
			  
			  PHINode *my_phi = Builder->CreatePHI(Builder->getInt1Ty(), 2, "phi");
			  my_phi->addIncoming(val1, info.expr);
			  my_phi->addIncoming(Builder->getInt1(1),  info.exit);
			  $$ = my_phi;  
			}

logical_AND_expression: inclusive_OR_expression
			{
			  $$ = $1;
			}
                        | logical_AND_expression LOGICAL_AND
			{ 
			  BasicBlock *first = BasicBlock::Create(M->getContext(), "SS.Check", Fun);
			  BasicBlock *exit =  BasicBlock::Create(M->getContext(), "SS.exit", Fun);
			  
			  Value *val = nullptr;
			  if($1->getType() == Builder->getFloatTy())
                          {
			     val = Builder->CreateFCmpONE($1, ConstantFP::get(Builder->getFloatTy(),0.0));
 			  }
			  else if($1->getType() == Builder->getInt32Ty())
                          {
			     val= Builder->CreateICmpNE($1, Builder->getInt32(0));
                          }			  
			  else if($1->getType() == Builder->getInt1Ty())
                          {
			     val = $1;
                          } 
                          else if(PointerType *py = dyn_cast<PointerType>($1->getType()) )
                          {
                             val = Builder->CreateICmpNE($1, ConstantPointerNull::get(py));
                          }
                          else
                             printf("Unknown operand in logical_And\n");
			    
			  Builder->CreateCondBr(val,first,exit);
			  push_loop(first, exit, NULL, Builder->GetInsertBlock());
			  Builder->SetInsertPoint(first);
			}
			inclusive_OR_expression
			{
			  Value *val1=nullptr;

			  if($4->getType() == Builder->getFloatTy())
                          {
			     val1 = Builder->CreateFCmpONE($4, ConstantFP::get(Builder->getFloatTy(),0.0));
                          }
			  else if($4->getType() == Builder->getInt32Ty())
                          {
			     val1 = Builder->CreateICmpNE($4, Builder->getInt32(0));
		          }
			  else if($4->getType() == Builder->getInt1Ty())
                          {
			     val1 = $4;
                          }
			  else if(PointerType *py = dyn_cast<PointerType>($4->getType()) )
                          {
                             val1 = Builder->CreateICmpNE($4, ConstantPointerNull::get(py));
                          }
                          else
                             printf("Unknown operand in logical_And\n");
			  

			  loop_info_t info = get_loop();
			  pop_loop();
			  Builder->CreateBr(info.body);
			  Builder->SetInsertPoint(info.body); 
			  
			  PHINode *my_phi = Builder->CreatePHI(Builder->getInt1Ty(), 2, "phi");
			  my_phi->addIncoming(val1, info.expr);
			  my_phi->addIncoming(Builder->getInt1(0),  info.exit);
			  $$ = my_phi;			  
			}
;

inclusive_OR_expression: exclusive_OR_expression
			{
			    $$=$1;
			}
                        | inclusive_OR_expression BITWISE_OR exclusive_OR_expression
			{
			  if(($1->getType() == Builder->getInt32Ty()) && ($3->getType() == Builder->getInt32Ty()))
				$$ = Builder->CreateOr($1,$3);
			  else
				parser_error("Invalid type of operand to '&'\n");
			}
;

exclusive_OR_expression: AND_expression
			{
			  $$ = $1;
			}
                        | exclusive_OR_expression BITWISE_XOR AND_expression
			{
			  if(($1->getType() == Builder->getInt32Ty()) && ($3->getType() == Builder->getInt32Ty()))
				$$ = Builder->CreateXor($1,$3);
			  else
				parser_error("Invalid type of operand to '&'\n");
			}
;

AND_expression:         equality_expression
			{
			  $$ = $1;
			}
                        | AND_expression AMPERSAND equality_expression
			{
			  if(($1->getType() == Builder->getInt32Ty()) && ($3->getType() == Builder->getInt32Ty()))
				$$ = Builder->CreateAnd($1,$3);
			  else
				parser_error("Invalid type of operand to '&'\n");
			}
;

equality_expression:    relational_expression
			{
			  $$ = $1;
			}
                        | equality_expression EQ relational_expression
			{
			  	if( (Builder->getFloatTy() == $1->getType()) || (Builder->getFloatTy() == $3->getType()) )
			  	{
				    Value *val = nullptr;
				    if(Builder->getInt32Ty() == $1->getType())
				    {
				       val = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOEQ(val,$3);
				    }
				    else if(Builder->getInt32Ty() == $3->getType())
				    {
				       val = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOEQ($1,val);
				    }
				    else
				       $$ = Builder->CreateFCmpOEQ($1,$3);
			  	}
			  	else
			    	    $$ = Builder->CreateICmpEQ($1,$3);
			}
                        | equality_expression NEQ relational_expression
			{
		          	if( (Builder->getFloatTy() == $1->getType()) || (Builder->getFloatTy() == $3->getType()) )
			  	{
				    Value *val = nullptr;
				    if(Builder->getInt32Ty() == $1->getType())
				    {
				       val = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpONE(val,$3);
				    }
				    else if(Builder->getInt32Ty() == $3->getType())
				    {
				       val = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpONE($1,val);
				    }
				    else
				       $$ = Builder->CreateFCmpONE($1,$3);
			  	}
				else
			    	   $$ = Builder->CreateICmpNE($1,$3);
			}
;

relational_expression:  shift_expression
			{
			    $$=$1;
			}
                        | relational_expression LT shift_expression
			{
			  if( (Builder->getFloatTy() == $1->getType()) || (Builder->getFloatTy() == $3->getType()) )
			  {
				    Value *val = nullptr;
				    if(Builder->getInt32Ty() == $1->getType())
				    {
				       val = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOLT(val,$3);
				    }
				    else if(Builder->getInt32Ty() == $3->getType())
				    {
				       val = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOLT($1,val);
				    }
				    else
				       $$ = Builder->CreateFCmpOLT($1,$3);
			   }
			   else
			    	   $$ = Builder->CreateICmpSLT($1,$3);
			}
                        | relational_expression GT shift_expression
			{
			  if( (Builder->getFloatTy() == $1->getType()) || (Builder->getFloatTy() == $3->getType()) )
			  {
				    Value *val = nullptr;
				    if(Builder->getInt32Ty() == $1->getType())
				    {
				       val = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOGT(val,$3);
				    }
				    else if(Builder->getInt32Ty() == $3->getType())
				    {
				       val = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOGT($1,val);
				    }
				    else
				       $$ = Builder->CreateFCmpOGT($1,$3);
			   }
			   else
			    	   $$ = Builder->CreateICmpSGT($1,$3);
			}
                        | relational_expression LTE shift_expression
			{
			  if( (Builder->getFloatTy() == $1->getType()) || (Builder->getFloatTy() == $3->getType()) )
			  {
				    Value *val = nullptr;
				    if(Builder->getInt32Ty() == $1->getType())
				    {
				       val = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOLE(val,$3);
				    }
				    else if(Builder->getInt32Ty() == $3->getType())
				    {
				       val = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOLE($1,val);
				    }
				    else
				       $$ = Builder->CreateFCmpOLE($1,$3);
			   }
			   else
			    	   $$ = Builder->CreateICmpSLE($1,$3);
			}     
                        | relational_expression GTE shift_expression
			{
			  if( (Builder->getFloatTy() == $1->getType()) || (Builder->getFloatTy() == $3->getType()) )
			  {
				    Value *val = nullptr;
				    if(Builder->getInt32Ty() == $1->getType())
				    {
				       val = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOGE(val,$3);
				    }
				    else if(Builder->getInt32Ty() == $3->getType())
				    {
				       val = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				       $$ = Builder->CreateFCmpOGE($1,val);
				    }
				    else
				       $$ = Builder->CreateFCmpOGE($1,$3);
			   }
			   else
			    	   $$ = Builder->CreateICmpSGE($1,$3);
			}
;

shift_expression:       additive_expression
			{
			    $$=$1;
			}
                        | shift_expression LSHIFT additive_expression
			{
			    if(($3->getType() == Builder->getInt32Ty())  && ($1->getType() == Builder->getInt32Ty()))
				$$ = Builder->CreateShl($1,$3);
			    else
				parser_error("Invalid type of operands to '<<'!");
			}
                        | shift_expression RSHIFT additive_expression
			{
			    if(($3->getType() == Builder->getInt32Ty())  && ($1->getType() == Builder->getInt32Ty()))
				$$ = Builder->CreateLShr($1,$3);
			    else
				parser_error("Invalid type of operands to '>>'!");
			}
;

additive_expression:    multiplicative_expression
			{
			  $$ = $1;
			}
                        | additive_expression PLUS multiplicative_expression
			{
			   if(($3->getType() == Builder->getFloatTy())  || ($1->getType() == Builder->getFloatTy()))
			   {
                                Value *val1 = $1, *val2 = $3;
				if($1->getType() != Builder->getFloatTy())
				   val1 = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				if($3->getType() != Builder->getFloatTy())
				   val2 = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				$$ = Builder->CreateFAdd(val1,val2);
			   }
			   else
				$$ = Builder->CreateAdd($1,$3);
			}
                        | additive_expression MINUS multiplicative_expression
			{
			   if(($3->getType() == Builder->getFloatTy())  || ($1->getType() == Builder->getFloatTy()))
			   {
				Value *val1 = $1, *val2 = $3;
				if($1->getType() != Builder->getFloatTy())
				   val1 = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				if($3->getType() != Builder->getFloatTy())
				   val2 = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				$$ = Builder->CreateFSub(val1,val2);
			   }
			   else
				$$ = Builder->CreateSub($1,$3);
			}
;

multiplicative_expression:  cast_expression
			{
			  $$ = $1;
			}
                        | multiplicative_expression STAR cast_expression
			{
			   if(($3->getType() == Builder->getFloatTy())  || ($1->getType() == Builder->getFloatTy()))
			   {
				Value *val1 = $1, *val2 = $3;
				if($1->getType() != Builder->getFloatTy())
				   val1 = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				if($3->getType() != Builder->getFloatTy())
				   val2 = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				$$ = Builder->CreateFMul(val1,val2);
			   }
			   else
				$$ = Builder->CreateMul($1,$3);
			}
                        | multiplicative_expression DIV cast_expression
			{
			   if(($3->getType() == Builder->getFloatTy())  || ($1->getType() == Builder->getFloatTy()))
			   {
				Value *val1 = $1, *val2 = $3;
				if($1->getType() != Builder->getFloatTy())
				   val1 = new SIToFPInst($1, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				if($3->getType() != Builder->getFloatTy())
				   val2 = new SIToFPInst($3, Builder->getFloatTy(), "", Builder->GetInsertBlock());
				$$ = Builder->CreateFDiv(val1,val2);
			   }
			   else
				$$ = Builder->CreateSDiv($1,$3);
			}
                        | multiplicative_expression MOD cast_expression
			{
 			   if(($3->getType() != Builder->getFloatTy())  && ($1->getType() != Builder->getFloatTy()))
			   {
				$$ = Builder->CreateSRem($1,$3);
			   }
			   else
				parser_error("Non integer type operands not allowed in '%' binary operator!");
			}
;

cast_expression:        unary_expression
			{ 
			  $$ = $1; 
			}
                        | LPAREN type_specifier RPAREN cast_expression
			{
			  if($2 == Builder->getInt32Ty())
			  {
			     if($4->getType() == Builder->getInt32Ty())
				$$ = $4;  
			     else if($4->getType() == Builder->getFloatTy())
				$$ = new FPToSIInst($4,Builder->getInt32Ty(), "", Builder->GetInsertBlock());  
			     else if(PointerType *py = dyn_cast<PointerType>($4->getType()))
				$$ = new PtrToIntInst($4,Builder->getInt32Ty());    
			  }
			  else if($2 == Builder->getFloatTy())
			  {
			     if($4->getType() == Builder->getFloatTy())
				$$ = $4;
			     else if($4->getType() == Builder->getInt32Ty())
				$$ = new SIToFPInst($4,Builder->getFloatTy(), "", Builder->GetInsertBlock());
			  }
			}
                        | LPAREN type_specifier STAR RPAREN cast_expression
			{
			   if($5->getType() == Builder->getInt32Ty())			   
			      $$ = Builder->CreateIntToPtr($5,PointerType::get(Builder->getInt32Ty(),0),"");
			   else
			      parser_error("Non integer types cannot be cast into pointers\n");			   
			}

;

lhs_expression:         ID 
			{
			  Value* val = symbol_find($1);
			  if (val==NULL)
			  {
			      parser_error("Symbol not defined yet");
			  }
			  else
                          {
			    printf("found symbol %s\n", $1);
			    $$ = val;
                          }
			}
                        | STAR ID
			{
			  Value* val = symbol_find($2);
			  if (val==NULL)
			    {
			      parser_error("Symbol not defined yet");
			    }
			  else
			    $$ = Builder->CreateLoad(val);
			}
;

unary_expression:       postfix_expression
			{
			  $$ = $1;
			}
			| AMPERSAND primary_expression
			{
			      if((dyn_cast<Instruction>$2)->hasAtomicLoad())
			      {  
				 $$ = (dyn_cast<Instruction>$2)->getOperand(0);
                                 (dyn_cast<Instruction>$2)->eraseFromParent();
			      }
			}
			| STAR primary_expression
			{
			  if($2->getType()->isPointerTy())
			  {   
			      Value *val = Builder->CreateLoad($2);
			      $$ = val;
			  }          
			  else
			  {
			     parser_error("Only pointer can be dereferenced! \n");
			  }
			}
                        | MINUS unary_expression
			{
			  $$ = Builder->CreateNeg($2);
			}
                        | PLUS unary_expression
			{
			  $$ = $2;
			}
                        | BITWISE_INVERT unary_expression
			{
			  if($2->getType() == Builder->getInt32Ty())
				$$ = Builder->CreateXor($2, llvm::ConstantInt::get(Builder->getInt32Ty(), 0xFFFFFFFF), "");
			  else
			  {
				parser_error("Cannot use '~' with non integer type variables");
			  }
			}
                        | NOT unary_expression
			{
				if($2->getType() == Builder->getInt32Ty())
				{
				   $$ = Builder->CreateZExt(Builder->CreateICmpEQ($2, Builder->getInt32(0)), Builder->getInt32Ty());
				}
				else if(PointerType *py = dyn_cast<PointerType>($2->getType()))
				{
				   $$ = Builder->CreateZExt(Builder->CreateICmpEQ($2, ConstantPointerNull::get(py)), Builder->getInt32Ty());
				}
				else if($2->getType() == Builder->getFloatTy())
				{
				   $$ = Builder->CreateZExt(Builder->CreateFCmpOEQ($2, ConstantFP::get(Builder->getFloatTy(),0.0)), Builder->getInt32Ty());
				}
			}
;

postfix_expression:     primary_expression
			{
			  $$ = $1;
			}
;

primary_expression:     ID 
			{ 
			  Value* val = symbol_find($1);
			  if (val==NULL) 
			  {
			      char *a=nullptr;
                              sprintf(a, "Symbol %s not yet declared", $1);
                              parser_error(a);
			  } 
			  else
                          {
			    $$ = Builder->CreateLoad(val);
                          }
			}
                        | constant
			{
			  $$ = $1;
			}
                        | LPAREN expression RPAREN
			{
			  $$ = $2;
			}
;

constant:	        CONSTANT_INTEGER  
			{ 
			  $$ = Builder->getInt32($1);
			} 
			| CONSTANT_FLOAT
			{
			  $$ = ConstantFP::get(Builder->getFloatTy(),$1);
			}
;

%%

Value* BuildFunction(Type* RetType, const char *name, 
			   paramlist_t *params)
{
  int i;
  int size = paramlist_size(params);

  std::vector<Type*> v;
  std::vector<const char*> vname;

  v.resize(size,NULL);
  vname.resize(size,NULL);

  paramlist_t *tmp = params;
  i=size-1;
  while(tmp)
    {
      v[i]=(tmp->type);
      vname[i]=(tmp->name);
      tmp = next_param(tmp);
      i--;
    }
  ArrayRef<Type*> Params(v);

  FunctionType* FunType = FunctionType::get(RetType,Params,false);

  Fun = Function::Create(FunType,GlobalValue::ExternalLinkage,
			 name,M);
  Twine T("entry");
  BasicBlock *BB = BasicBlock::Create(M->getContext(),T,Fun);

  /* Create an Instruction Builder */
  Builder = new IRBuilder<>(M->getContext());
  Builder->SetInsertPoint(BB);

  Function::arg_iterator I = Fun->arg_begin();
  for(i=0; I!=Fun->arg_end();i++, I++)
    {
      // map args and create allocas!
      AllocaInst *AI = Builder->CreateAlloca(v[i]);
      Builder->CreateStore(&(*I),(Value*)AI);
      symbol_insert(vname[i],(Value*)AI);
    }


  return Fun;
}

extern int line_num;
extern char *infile[];
static int   infile_cnt=0;
extern FILE * yyin;

int parser_error(const char *msg)
{
  printf("%s (%d): Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int internal_error(const char *msg)
{
  printf("%s (%d): Internal Error -- %s\n",infile[infile_cnt-1],line_num,msg);
  return 1;
}

int yywrap() {
  static FILE * currentFile = NULL;

  if ( (currentFile != 0) ) {
    fclose(yyin);
  }
  
  if(infile[infile_cnt]==NULL)
    return 1;

  currentFile = fopen(infile[infile_cnt],"r");
  if(currentFile!=NULL)
    yyin = currentFile;
  else
    printf("Could not open file: %s",infile[infile_cnt]);

  infile_cnt++;
  
  return (currentFile)?0:1;
}

int yyerror(const char* error)
{
  parser_error("Un-resolved syntax error.");
  return 1;
}

char * get_filename()
{
  return infile[infile_cnt-1];
}

int get_lineno()
{
  return line_num;
}


void minic_abort()
{
  parser_error("Too many errors to continue.");
  exit(1);
}
