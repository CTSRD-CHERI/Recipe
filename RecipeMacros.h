#ifndef _RECIPE_MACROS_H_
#define _RECIPE_MACROS_H_

#define Seq rSeq(rBlock(
#define Par rPar(rBlock(
#define FastSeq rFastSeq(rBlock(
#define If(a) rIfElse(a,(
#define Else ),(
#define While(a) rWhile(a, (
#define End ))

#endif
