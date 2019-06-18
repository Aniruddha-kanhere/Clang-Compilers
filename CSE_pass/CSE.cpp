/*
 * File: CSE_Cpp.cpp
 */

/* LLVM Header Files */
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Type.h"
#include "llvm/Analysis/AliasAnalysis.h"
#include "llvm/IR/Dominators.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/IR/InstIterator.h"

#include <map>

using namespace llvm;

// ^^^^
// must be after using namespace llvm; sorry for the ugly code
#include "CSE.h"
#include "dominance.h"
#include "transform.h"

int CSEDead=0;
int CSEElim=0;
int CSESimplify=0;
int CSELdElim=0;
int CSELdStElim=0;
int CSERStElim=0;

void Calculate_CSE(BasicBlock::iterator Ins_it, BasicBlock *BB_it)
{
  llvm::Instruction *I = &*Ins_it;

  //If the instruction is a load/store/call/branch then do nothing and move ahead
  if(isa<llvm::TerminatorInst>(I) ||          //If TerminatorInst
     isa<llvm::VAArgInst>(I)      ||          //if VAArgInst
     isa<llvm::BranchInst>(I)     ||          //If Branch
     isa<llvm::FCmpInst>(I)       ||          //If FCmpInst
     LLVMIsAAllocaInst(wrap(I))   ||          //if Alloca
     LLVMIsACallInst(wrap(I))     ||          //if call
     LLVMIsAStoreInst(wrap(I))    ||          //if store
     LLVMIsALoadInst(wrap(I)) )               //if load
    ;
  else
  {
    auto child = unwrap(LLVMFirstDomChild(wrap(&*BB_it)));

    while(child)
    {
      for(auto ins_it2 = child->begin() ; ins_it2!=child->end();)
      {
        if(ins_it2->isIdenticalTo(I))
        {
          CSEElim++;
          ins_it2->replaceAllUsesWith(I);
          ins_it2 = ins_it2->eraseFromParent();
        }
        else
        {
          ins_it2++;
        }
      }

      //Recursively call This till we ttraverse all the child nodes
      Calculate_CSE(Ins_it, child);

      //Update the child node
      child = unwrap(LLVMNextDomChild(wrap(&*BB_it), wrap(&*child)));
    }
  }
}

void LLVMCommonSubexpressionElimination_Cpp(Module *M)
{
  for(auto Function_it = M->begin(); Function_it!= M->end(); Function_it++)
  {
    for(auto BasicBlock_it = Function_it->begin(); BasicBlock_it!=Function_it->end(); BasicBlock_it++)
    {
      for(auto Instruction_it=BasicBlock_it->begin(); Instruction_it!=BasicBlock_it->end(); /*Instruction_it++*/)
      {
        bool Inst_count_updated=0;
        llvm::Instruction *I = &*Instruction_it;

        //If the instruction is a load/store/call/branch then do nothing and move ahead
        if(isa<llvm::TerminatorInst>(I) ||          //If TerminatorInst
           isa<llvm::VAArgInst>(I)      ||          //if VAArgInst
           isa<llvm::BranchInst>(I)     ||          //If Branch
           isa<llvm::FCmpInst>(I)       ||          //If FCmpInst
           LLVMIsAAllocaInst(wrap(I))   ||          //if Alloca
           LLVMIsACallInst(wrap(I))     ||          //if call
           LLVMIsAStoreInst(wrap(I))    ||          //if store
           LLVMIsALoadInst(wrap(I)) )               //if load
          ;
        else
        {
          //Common Subexpression Elimination Pass (Basic)
          {//=============================================== CSE Basic Start ===================================================
            //Iterate over the following instructions of the current basic block
            auto ins_it2 = Instruction_it;
            ins_it2++;      //increment so we don't have the current instruction
            for(; ins_it2!=BasicBlock_it->end();)
            {
              if(ins_it2->isIdenticalTo(I))
              {
                CSEElim++;
                ins_it2->replaceAllUsesWith(I);
                ins_it2 = ins_it2->eraseFromParent();
              }
              else
              {
                ins_it2++;
              }
            }

            //Start iterating over the child blocks
            Calculate_CSE(Instruction_it,&*BasicBlock_it);
          }//===============================================  CSE Basic End  ===================================================

          //Optimisation 0
          {//=============================================== Dead Code Elimination ==============================================
              if(I->use_empty())
              {
                CSEDead++;
                Instruction_it = Instruction_it->eraseFromParent();
                //Instruction *temp = I;
                //Instruction_it++;
                Inst_count_updated=1;
                //temp->eraseFromParent();
              }
          }//====================================================DCE Ends =======================================================


          {//==================================================Optimisation 1====================================================
            Value *temp = unwrap(InstructionSimplify(wrap(&*Instruction_it)));
            if(temp != NULL)
            {
              Instruction_it->replaceAllUsesWith(temp);
              Inst_count_updated=1;
              Instruction_it=Instruction_it->eraseFromParent();
              CSESimplify++;
            }
          }//=======================================================Opt 1 end======================================================
        }

        if(LLVMIsALoadInst(wrap(I)))
        {
          {//====================================================Optimisation 2====================================================
            auto Opt2_ins_it=Instruction_it;
            if(!Inst_count_updated)
              Opt2_ins_it++;
            for(; Opt2_ins_it!=BasicBlock_it->end();)
            {
              if(LLVMIsAStoreInst(wrap(&*Opt2_ins_it)))
                break;

              if(LLVMIsALoadInst(wrap(&*Opt2_ins_it)) && !(dyn_cast<LoadInst>(Opt2_ins_it)->isVolatile())
                     && I->getType()==Opt2_ins_it->getType() && I->getOperand(0)==Opt2_ins_it->getOperand(0))
              {
                CSELdElim++;
                Opt2_ins_it->replaceAllUsesWith(I);
                Opt2_ins_it = Opt2_ins_it->eraseFromParent();
                //Opt2_ins_it++;
              }
              else
              {
                Opt2_ins_it++;
              }
            }
          }//=========================================================Opt 2 end========================================================
        }

        if(LLVMIsAStoreInst(wrap(I)))
        {
          {//=======================================================Optimisation 3======================================================
            auto Opt3_ins_it=Instruction_it;
            if(!Inst_count_updated)
              Opt3_ins_it++;
            for(; Opt3_ins_it!=BasicBlock_it->end();)
            {
              if(LLVMIsALoadInst(wrap(&*Opt3_ins_it)) && !(dyn_cast<llvm::LoadInst>(Opt3_ins_it)->isVolatile())
                    && I->getOperand(1)==Opt3_ins_it->getOperand(0) && I->getOperand(0)->getType()==Opt3_ins_it->getType())
              {
                CSELdStElim++;
                Opt3_ins_it->replaceAllUsesWith(I->getOperand(0));
                Opt3_ins_it=Opt3_ins_it->eraseFromParent();
                //Opt3_ins_it++;
              }
              else if(LLVMIsAStoreInst(wrap(&*Opt3_ins_it)) && I->getOperand(1)==Opt3_ins_it->getOperand(1)
                          && !(dyn_cast<llvm::StoreInst>(I)->isVolatile()) && I->getOperand(0)->getType()==Opt3_ins_it->getOperand(0)->getType())
              {
                Instruction_it=Instruction_it->eraseFromParent();
                Inst_count_updated=1;
                CSERStElim++;
                break;
              }
              else if(LLVMIsALoadInst(wrap(&*Opt3_ins_it)) || LLVMIsAStoreInst(wrap(&*Opt3_ins_it)))
              {
                break;
              }
              else
              {
                Opt3_ins_it++;
              }
            }
          }//==========================================================Opt 3 end==============================================================
        }


        //if any of the Optimizations haven't updated the instruction count, then increment it
        if(!Inst_count_updated)
          Instruction_it++;
      }
    }
  }

  // print out summary of results
  fprintf(stderr,"CSE_Dead.....%d\n", CSEDead);
  fprintf(stderr,"CSE_Basic.....%d\n", CSEElim);
  fprintf(stderr,"CSE_Simplify..%d\n", CSESimplify);
  fprintf(stderr,"CSE_RLd.......%d\n", CSELdElim);
  fprintf(stderr,"CSE_RSt.......%d\n", CSERStElim);
  fprintf(stderr,"CSE_LdSt......%d\n", CSELdStElim);
}
