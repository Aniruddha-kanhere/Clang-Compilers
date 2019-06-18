/*
 * File: summary.cpp
 *
 * Description:
 *   This is where you implement your project 3 support.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>

/* LLVM Header Files */
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/User.h"
#include "llvm-c/Core.h"
#include <map>

/* LLVM Header Files */
#include "dominance.h"

/* Header file global to this project */
#include "Summary.hpp"

#define DEBUG 0

std::map<llvm::BasicBlock *, int> my_map;

using namespace llvm;

typedef struct Stats_def {
  int functions;                   //Number of functions in the module
  int globals;                     //Number of Gloabal variables in the module
  int bbs;                         //Number of BasicBlocks in the module

  int insns;                       //Total number of instruction in the module
  int insns_nearby_dep;            //Instructions using operands defined in the same BasicBlock

  int allocas;                     //Total allocas in the Module`

  int loads;                       //Total number of Loads in the Module
  int loads_alloca;                //Loads which load from an alloca
  int loads_globals;               //Loads which load from a global

  int stores;                       //Total number of stores in the module
  int stores_alloca;                //Stores storing to an alloca
  int stores_globals;               //Stores storing to a global variable

  int conditional_branches;         //Number of conditional branches
  int calls;                        //Number of calls to functions
  int calls_with_pointers;          //Calls with atleast one pointer operand
  int calls_within_same_mod;        //Calls to functions defined within the same module

  int gep;                          //Number of GEP instructions
  int gep_load;                     //Number of GEPs using a load as base pointer
  int gep_alloca;                   //Number of GEPs using an alloca as base pointer
  int gep_globals;                  //Number of GEPs using a global variable as base pointer
  int gep_gep;                      //Number of GEPs using another GEP as base pointer

  int loops;                        //Number of loops - approximated by backedges
  int floats;                       //Instructions using atleast one f32/f64 operand
} Stats;

void pretty_print_stats(Stats s, int spaces)
{
  char spc[128];
  int i;

  // insert spaces before each line
  for(i=0; i<spaces; i++)
    spc[i] = ' ';
  spc[i] = '\0';

  printf("%sFunctions.......................%d\n",spc,s.functions);
  printf("%sGlobal Vars.....................%d\n",spc,s.globals);
  printf("%sBasic Blocks....................%d\n",spc,s.bbs);
  printf("%sInstructions....................%d\n",spc,s.insns);

  printf("%sInstructions - Cond. Branches...%d\n",spc,s.conditional_branches);
  printf("%sInstructions - Calls............%d\n",spc,s.calls);
  printf("%sInstructions - Calls.w.ptr......%d\n",spc,s.calls_with_pointers);
  printf("%sInstructions - Calls-same-mod...%d\n",spc,s.calls_within_same_mod);

  printf("%sInstructions - Allocas..........%d\n",spc,s.allocas);
  printf("%sInstructions - Loads............%d\n",spc,s.loads);
  printf("%sInstructions - Loads (alloca)...%d\n",spc,s.loads_alloca);
  printf("%sInstructions - Loads (globals)..%d\n",spc,s.loads_globals);

  printf("%sInstructions - Stores...........%d\n",spc,s.stores);
  printf("%sInstructions - Stores (alloca)..%d\n",spc,s.stores_alloca);
  printf("%sInstructions - Stores (globals).%d\n",spc,s.stores_globals);

  printf("%sInstructions - gep..............%d\n",spc,s.gep);
  printf("%sInstructions - gep (load).......%d\n",spc,s.gep_load);
  printf("%sInstructions - gep (alloca).....%d\n",spc,s.gep_alloca);
  printf("%sInstructions - gep (globals)....%d\n",spc,s.gep_globals);
  printf("%sInstructions - gep (gep)........%d\n",spc,s.gep_gep);

  printf("%sInstructions - Other............%d\n",spc,
	  s.insns-s.conditional_branches-s.loads-s.stores-s.gep-s.calls);
  printf("%sLoops...........................%d\n",spc,s.loops);
  printf("%sFloats..........................%d\n",spc,s.floats);
}

void print_csv_file(const char *filename, Stats s, const char *id)
{
  FILE *f = fopen(filename,"w");
  fprintf(f,"id,%s\n",id);
  fprintf(f,"functions,%d\n",s.functions);
  fprintf(f,"globals,%d\n",s.globals);
  fprintf(f,"bbs,%d\n",s.bbs);
  fprintf(f,"insns,%d\n",s.insns);
  fprintf(f,"allocas,%d\n",s.allocas);
  fprintf(f,"branches,%d\n",s.conditional_branches);
  fprintf(f,"calls,%d\n",s.calls);
  fprintf(f,"calls_w_ptr,%d\n",s.calls_with_pointers);
  fprintf(f,"loads,%d\n",s.loads);
  fprintf(f,"loads_alloca,%d\n",s.loads_alloca);
  fprintf(f,"loads_globals,%d\n",s.loads_globals);
  fprintf(f,"stores,%d\n",s.stores);
  fprintf(f,"stores_alloca,%d\n",s.stores_alloca);
  fprintf(f,"stores_global,%d\n",s.stores_globals);
  fprintf(f,"gep,%d\n",s.gep);
  fprintf(f,"gep_load,%d\n",s.gep_load);
  fprintf(f,"gep_alloca,%d\n",s.gep_alloca);
  fprintf(f,"gep_globals,%d\n",s.gep_globals);
  fprintf(f,"gep_gep,%d\n",s.gep_gep);
  fprintf(f,"loops,%d\n",s.loops);
  fprintf(f,"floats,%d\n",s.floats);
  fprintf(f,"funct_same_mod, %d\n", s.calls_within_same_mod);
  fprintf(f,"insns_nearby,%d\n", s.insns_nearby_dep);
  fclose(f);
}

Stats MyStats;

void Summarize(llvm::Module *M, const char *id, const char *filename)
{
  for(auto git = M->global_begin(); git!=M->global_end(); git++)
  {
    if(git->hasInitializer())
    {
      #if DEBUG
      printf("Global Var: ");
      LLVMDumpValue(wrap(&*git));
      printf("\n");
      #endif
      MyStats.globals++;
    }
  }

  for(auto fit = M->begin(); fit!= M->end(); fit++)
  {
    if(fit->size())
  	{
      #if DEBUG
      printf("Size of this fucntion = %zu\n", fit->size());
      #endif
  		MyStats.functions++;
  		for(auto bit = fit->begin(); bit!= fit->end(); bit++)
  		{
  			MyStats.bbs++;

  			for(auto iit = bit->begin(); iit!= bit->end(); iit++)
  			{
  				MyStats.insns++;

          llvm::Instruction *I = &*iit;

          /*-----------If the instruction has operands which are either float or double ------------------*/
          for(unsigned int op_cnt = 0; op_cnt < I->getNumOperands(); op_cnt++)
          {
            if( I->getOperand(op_cnt)->getType()->isFloatTy()  )
            {
              MyStats.floats++;
              //LLVMDumpValue(wrap(I));
              //printf("\n");
              break;
            }
            else if( I->getOperand(op_cnt)->getType()->isDoubleTy()  )
            {
              MyStats.floats++;
              //LLVMDumpValue(wrap(I));
              //printf("\n");
              break;
            }
          }

          /*-----------If the instruction has operands defined within the same basic block-----------------*/
          for(unsigned int op_cnt = 0; op_cnt < I->getNumOperands(); op_cnt++)
          {
            Value *v = I->getOperand(op_cnt);
            /*--------------------It must be an instruction first to be defined-----------------------------*/
            if(isa<llvm::Instruction>(v))
            {
              if(dyn_cast<llvm::Instruction>(v)->getParent() == &*bit)
              {
                MyStats.insns_nearby_dep++;
                break;
              }
            }
          }

          /*------------------------------ If this is a branch instruction -------------------------------*/
  				if(isa<llvm::BranchInst>(I))
          {
            #if DEBUG
            printf("It is a branch\n");
            #endif
            /*------------------------- if its unconditional, then is it a loop?--------------------------*/
            if((dyn_cast<llvm::BranchInst>(I))->isUnconditional())
            {
              llvm::BasicBlock *Dest_head1 = cast<llvm::BasicBlock>(I->getOperand(0));
              if( LLVMDominates( wrap(&*fit), wrap(&*Dest_head1), wrap(&*bit) ) )
              {
                if(my_map.find(Dest_head1) == my_map.end())
                {
                  #if DEBUG
                  printf("%d: Unconditional Branch and loop \n",MyStats.insns);
                  #endif
                  my_map[Dest_head1] = 1;
                  MyStats.loops++;
                }
              }
            }
            /*--------------------------if it is conditional, then is it a loop?---------------------------*/
            else if((dyn_cast<llvm::BranchInst>(I))->isConditional())
            {
              MyStats.conditional_branches++;

              llvm::BasicBlock *Dest_head1 = cast<llvm::BasicBlock>(I->getOperand(1));
              llvm::BasicBlock *Dest_head2 = cast<llvm::BasicBlock>(I->getOperand(2));

              if( LLVMDominates( wrap(&*fit), wrap(&*Dest_head1), wrap(&*bit) ) )
              {
                #if DEBUG
                printf("%d: Conditional Branch and loop \n",MyStats.insns);
                #endif
                if(my_map.find(Dest_head1) == my_map.end())
                {
                  my_map[Dest_head1] = 1;
                  MyStats.loops++;
                }
              }

              if( LLVMDominates( wrap(&*fit), wrap(&*Dest_head2), wrap(&*bit) ) )
              {
                #if DEBUG
                printf("%d: Conditional Branch and loop \n",MyStats.insns);
                #endif
                //if(my_map.find(Dest_head2) == my_map.end())
                {
                  //my_map[Dest_head2] = 1;
                  MyStats.loops++;
                }
              }
            }
          }
          /*---------------------------------Is this a Call Instruction?--------------------------------------*/
          if(LLVMIsACallInst(wrap(I)))
          {
            #if DEBUG
            printf("It is a call with operands = %d\n",I->getNumOperands());
            #endif
            MyStats.calls++;

            /*-----------If the call instruction uses a pointers to get its arguments-----------------*/
            for(unsigned int op_cnt = 0; op_cnt < ( I->getNumOperands()-1 ); op_cnt++)
            {
              if(I->getOperand(op_cnt)->getType()->isPointerTy())// isa<llvm::PointerType>(I->getOperand(op_cnt)->getType()))
              {
                #if DEBUG
                printf(" This operand is a pointer  ");
                LLVMDumpValue(wrap(I->getOperand(op_cnt)));
                printf("\n");
                #endif
                MyStats.calls_with_pointers++;
                break;
              }
            }

            if( Function *f = dyn_cast<Function> ( I->getOperand(I->getNumOperands()-1) ) )
            {
              #if DEBUG
              printf("Casting Successful\n");
              #endif

              if(f->size())
              {
                #if DEBUG
                printf("Same module\n");
                #endif
                MyStats.calls_within_same_mod++;
              }
            }
          }
          if(LLVMIsAAllocaInst(wrap(I))) //I->getOpcode() == LLVMAlloca) //Instruction::Alloca)
          {
            #if DEBUG
            printf("It is an alloca\n");
            #endif
            MyStats.allocas++;
          }
          if(LLVMIsALoadInst(wrap(I)))
          {
            #if DEBUG
            printf("It is a load\n");
            #endif
            MyStats.loads++;

            //------------------ Is it a load from alloca?---------------------------
            if(LLVMIsAAllocaInst(wrap(I->getOperand(0))))
            {
              MyStats.loads_alloca++;
            }
            //---------------- Or is a load from a global Variable -------------------
            else if(isa<GlobalVariable>(I->getOperand(0)))
            {
              MyStats.loads_globals++;
            }
          }
          /*---------------------- Is this instruction a Store? ----------------------*/
          if(LLVMIsAStoreInst(wrap(I)))
          {
            #if DEBUG
            printf("It is a store\n");
            #endif
            MyStats.stores++;
            //---------------Is the operand from a previous alloca? ------------------
            if(LLVMIsAAllocaInst(wrap(I->getOperand(1))))
            {
              MyStats.stores_alloca++;
            }
            //---------------- Or is a store to a global Variable -------------------
            else if(isa<GlobalVariable>(I->getOperand(1)))
            {
              MyStats.stores_globals++;
            }

          }
          //----------------------- Is this a GEP instruction? ------------------------
          if(LLVMIsAGetElementPtrInst(wrap(I)))
          {
            #if DEBUG
            printf("Get Element Pointer Instr\n");
            #endif
            MyStats.gep++;

            #if DEBUG
            LLVMDumpValue(wrap(I->getOperand(0)));
            printf("\n");
            #endif

            //--------------- What is it using as its base pointer? ----------------
            //-------- A load? ---------
            if(LLVMIsALoadInst( wrap (dyn_cast<Instruction>(I->getOperand(0)) ) ))
            {
              #if DEBUG
              printf("GEP from a Load\n");
              #endif
              MyStats.gep_load++;
            }
            //-------- or An alloca? ---------
            if(LLVMIsAAllocaInst( wrap (dyn_cast<Instruction>(I->getOperand(0)) ) ))
            {
              #if DEBUG
              printf("GEP from a Alloca\n");
              #endif
              MyStats.gep_alloca++;
            }
            //-------- or a global? ---------
            if( isa<GlobalVariable>(I->getOperand(0)) )
            {
              #if DEBUG
              printf("GEP from a Global\n");
              #endif
              MyStats.gep_globals++;
            }
            //-------- or a second GEP? ---------
            if(LLVMIsAGetElementPtrInst( wrap (dyn_cast<Instruction>(I->getOperand(0)) ) ))
            {
              #if DEBUG
              printf("GEP from another GEP\n");
              #endif
              MyStats.gep_gep++;
            }
          }
  			}
  		}
  	}
  }

  #if DEBUG
  pretty_print_stats(MyStats,4);
  #endif
  print_csv_file(filename,MyStats,id);
}
