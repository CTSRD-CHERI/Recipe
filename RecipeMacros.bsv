`ifndef _RECIPE_MACROS_
`define _RECIPE_MACROS_

`define Seq(block) rSeq(rBlock block)
`define Par(block) rPar(rBlock block)
`define FastSeq(block) rFastSeq(rBlock block)
`define If(a, b, c) rIfElse(a, b, c)
`define While(a, b) rWhile(a, b)

`endif
