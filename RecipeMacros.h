#ifndef _RECIPE_MACROS_H_
#define _RECIPE_MACROS_H_

#define Seq rSeq(rBlock(
#define Par rPar(rBlock(
#define FastSeq rFastSeq(rBlock(
#define If(a) rIfElse(a,toRecipe(
#define Else ),toRecipe(
#define While(a) rWhile(a,toRecipe(
#define End ))

#endif
