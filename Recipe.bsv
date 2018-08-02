/*-
 * Copyright (c) 2018 Alexandre Joannou
 * Copyright (c) 2018 Matthew Naylor
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package Recipe;

// list of imported packages
import List :: *;
import FIFO :: *;
import GetPut :: *;
import Connectable :: * ;
import SpecialFIFOs :: *;

// list of exported indentifiers
export Recipe;
export rAct;
export rActV;
export rFastSeq;
export rSeq;
export rPar;
export rIfElse;
export rWhen;
export rWhile;
export rBlock;
export RecipeBlock;
export RecipeFSM(..);
export compile;
export compileMutuallyExclusive;
export compileRules;

// Flow control
typedef FIFO#(Bit#(0)) FlowFF;
// Recipe data type
typedef union tagged {
  Action Act; // Basic Action recipe
  ActionValue#(Bool) ActV; // ActionValue recipe
  List#(Recipe) Par; // All recipes happen in parallel
  List#(Recipe) Seq; // All recipes happen in order with one cycle latency (separated by mkFIFO1)
  // XXX FastSeq:
  // All recipes happen in order with no latency (separated by mkBypassFIFO).
  // Data dependencies can still create latency. Any module whose firing in an early rule depends
  // on the firing of a later rule (typically mkPipeLineFIFO and the likes) will cause a scheduling
  // error as the rules are separated by a mkBypassFIFO, leading to a cycle in the canfire/willfire signals.
  List#(Recipe) FastSeq;
  Tuple3#(Bool, Recipe, Recipe) IfElse; // First recipe happens for a True condition, second recipe otherwise
  Tuple2#(Bool, Recipe) While; // Recipe happens as long as the condition is True
} Recipe;

// Recipe block constructor (to wrap multiple recipes in a list, passed to Par/Seq/FastSeq)
typeclass RecipeBlock#(type a);
  function a mkRecipe(List#(Recipe) acc);
endtypeclass

instance RecipeBlock#(List#(Recipe));
  function mkRecipe(acc) = reverse(acc);
endinstance

instance RecipeBlock#(function a f(Action elem)) provisos (RecipeBlock#(a));
  function mkRecipe(acc, elem) = mkRecipe(cons(Act(elem), acc));
endinstance

instance RecipeBlock#(function a f(ActionValue#(Bool) elem)) provisos (RecipeBlock#(a));
  function mkRecipe(acc, elem) = mkRecipe(cons(ActV(elem), acc));
endinstance

instance RecipeBlock#(function a f(Recipe elem)) provisos (RecipeBlock#(a));
  function mkRecipe(acc, elem) = mkRecipe(Cons(elem, acc));
endinstance

function a rBlock() provisos (RecipeBlock#(a));
  return mkRecipe(Nil);
endfunction

// Recipe constructor wrapper function
function Recipe rAct(Action a) = Act(a);
function Recipe rActV(ActionValue#(Bool) a) = ActV(a);
function Recipe rPar(List#(Recipe) rs) = Par(rs);
function Recipe rSeq(List#(Recipe) rs) = Seq(rs);
function Recipe rFastSeq(List#(Recipe) rs) = FastSeq(rs);
function Recipe rIfElse(Bool c, Recipe r0, Recipe r1) = IfElse(tuple3(c, r0, r1));
function Recipe rWhen(Bool c, Recipe r) = IfElse(tuple3(c, r, Act(noAction)));
function Recipe rWhile(Bool c, Recipe r) = While(tuple2(c, r));

/* TODO proprocessor macros to apply rBlock implicitly
#define rAct(a) Act(a)
#define rActV(a) ActV(a)
#define rPar(...) Par(rBlock(__VA_ARGS__))
#define rSeq(...) Seq(rBlock(__VA_ARGS__))
#define rFastSeq(...) FastSeq(rBlock(__VA_ARGS__))
#define rIfElse(a, b, c) IfElse(tuple3(a, b, c))
#define rWhile(a, b) While(tuple2(a, b))
*/

// Recipe compiler front modules
////////////////////////////////////////////////////////////////////////////////

interface RecipeFSM;
  method Action start();
  method Bool isLastCycle();
  method Bool isDone();
  method Action waitForDone();
endinterface

// compiles a recipe and returns the generated rules together with a done signal
module [Module] compileRules#(Recipe r) (Tuple2#(Rules, RecipeFSM));
  Tuple2#(Rules, RecipeFSM) x <- topCompile(r);
  return x;
endmodule
// compiles a recipe, adds the rules to the module and returns a done signal
module [Module] compile#(Recipe r) (RecipeFSM);
  Tuple2#(Rules, RecipeFSM) x <- topCompile(r);
  addRules(tpl_1(x));
  return tpl_2(x);
endmodule
// compiles a list of recipes, adds the rules for each recipe such that they are
// mutually exclusive, and returns a list of done signals
module [Module] compileMutuallyExclusive#(List#(Recipe) rs) (List#(RecipeFSM));
  List#(Tuple2#(Rules, RecipeFSM)) xs <- mapM(topCompile, rs);
  addRules(fold(rJoinMutuallyExclusive, map(tpl_1, xs)));
  return map(tpl_2,xs);
endmodule

// Recipe compiler inner modules
////////////////////////////////////////////////////////////////////////////////

// top compile module wrapping the machine with appropriate flow control
module [Module] topCompile#(Recipe r) (Tuple2#(Rules, RecipeFSM));
  FlowFF goFF <- mkBypassFIFO;
  FlowFF busyFF <- mkBypassFIFO; // To push back while the machine is busy
  FlowFF doneFF <- mkBypassFIFO;
  Reg#(Bool) doneReg[3] <- mkCReg(3, False);
  PulseWire lastCycle <- mkPulseWire;
  // compile recipe and gather rules
  Rules compiledRules <- innerCompile(r, goFF, doneFF);
  Rules wrappingRules = rules
    rule endMachine;
      //$display("%0t -- endMachine", $time);
      doneFF.deq();
      busyFF.deq();
      lastCycle.send();
      doneReg[1] <= True;
    endrule
  endrules;
  // implement RecipeFSM interface
  module mkIFC (RecipeFSM);
    method Action start() = action
      doneReg[0] <= False;
      goFF.enq(?);
      busyFF.enq(?);
    endaction;
    method Bool isLastCycle() = lastCycle;
    method Bool isDone() = doneReg[2];
    method Action waitForDone() if (doneReg[2]) = action endaction;
  endmodule
  RecipeFSM machine <- mkIFC;
  return tuple2(rJoin(wrappingRules, compiledRules), machine);
endmodule

// main compile module
module [Module] innerCompile#(Recipe r, FlowFF goFF, FlowFF doneFF) (Rules);

  // module helper wrapping Recipe output with a provided FlowFF constructor
  //////////////////////////////////////////////////////////////////////////////
  module [Module] compileWrapOut#(Module#(FlowFF) mkFF, Recipe r, FlowFF inFF) (Tuple2#(FlowFF, Rules));
    FlowFF outFF <- mkFF;
    Rules innerRules <- innerCompile(r, inFF, outFF);
    return tuple2(outFF, innerRules);
  endmodule
  // sequence module helpers
  //////////////////////////////////////////////////////////////////////////////
  module emptySeq (Rules);
    return rules
      rule emptySeq;
        goFF.deq();
        doneFF.enq(?);
      endrule
    endrules;
  endmodule
  module [Module] nonEmptySeq#(Module#(FlowFF) mkFF, List#(Recipe) rs) (Rules);
    FlowFF lastFF = goFF;
    Integer seqLength = length(rs);
    List#(Recipe) rList = rs;
    // go through the list and leave the last element in there
    Rules seqRules = emptyRules;
    for (Integer i = 0; i < seqLength - 1; i = i + 1) begin
      Tuple2#(FlowFF, Rules) step <- compileWrapOut(mkFF, head(rList), lastFF);
      lastFF = tpl_1(step);
      seqRules = rJoin(seqRules, tpl_2(step));
      rList = tail(rList);
    end
    Rules lastStep <- innerCompile(head(rList), lastFF, doneFF);
    return rJoin(seqRules, lastStep);
  endmodule
  // core compiler
  //////////////////////////////////////////////////////////////////////////////
  case (r) matches
    // Action recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged Act .a: return rules
      rule runAct;
        a;
        goFF.deq();
        doneFF.enq(?);
      endrule
    endrules;
    // ActionValue recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged ActV .av: return rules
      rule runActV;
        Bool isDone <- av;
        if (isDone) begin
          goFF.deq();
          doneFF.enq(?);
        end
      endrule
    endrules;
    // Par recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged Par .rs: begin
      Integer parLength = length(rs);
      List#(FlowFF) inFFs <- replicateM(parLength, mkBypassFIFO);
      List#(Tuple2#(FlowFF, Rules)) branches <- zipWithM (compileWrapOut(mkBypassFIFO), rs, inFFs);
      Rules branchRules = fold(rJoin, map(tpl_2, branches));
      Rules forkRule = rules rule forkPar;
        goFF.deq();
        function Action doEnq(FIFO#(x) ff) = action ff.enq(?); endaction;
        joinActions(map(doEnq, inFFs));
      endrule endrules;
      Rules joinRule = rules rule joinPar;
        function Action doDeq(FIFO#(x) ff) = action ff.deq(); endaction;
        joinActions(map(doDeq, map(tpl_1, branches)));
        doneFF.enq(?);
      endrule endrules;
      return rJoin(forkRule, rJoin(branchRules, joinRule));
    end
    // Seq recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged Seq Nil: begin Rules x <- emptySeq; return x; end
    tagged Seq .rs: begin Rules x <- nonEmptySeq(mkFIFO1, rs); return x; end
    // FastSeq recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged FastSeq Nil: begin Rules x <- emptySeq; return x; end
    tagged FastSeq .rs: begin Rules x <- nonEmptySeq(mkBypassFIFO, rs); return x; end
    // If / Else recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged IfElse {.cond, .r_if, .r_else}: begin
      // declare flow control state //
      ////////////////////////////////
      FlowFF inFF_if     <- mkBypassFIFO;
      FlowFF inFF_else   <- mkBypassFIFO;
      FlowFF outFF_if    <- mkBypassFIFO;
      FlowFF outFF_else  <- mkBypassFIFO;
      FIFO#(Bool) condFF <- mkBypassFIFO;
      // compile and gather rules //
      //////////////////////////////
      Rules ifRules   <- innerCompile(r_if, inFF_if, outFF_if);
      Rules elseRules <- innerCompile(r_else, inFF_else, outFF_else);
      Rules beforeRules = rules rule enqCondFF;
        goFF.deq();
        condFF.enq(cond);
        if (cond) inFF_if.enq(?);
        else inFF_else.enq(?);
      endrule endrules;
      Rules afterIfRules = rules rule finishIf(condFF.first);
        condFF.deq();
        outFF_if.deq();
        doneFF.enq(?);
      endrule endrules;
      Rules afterElseRules = rules rule finishElse(!condFF.first);
        condFF.deq();
        outFF_else.deq();
        doneFF.enq(?);
      endrule endrules;
      // join rules //
      ////////////////
      ifRules = rJoin(ifRules, afterIfRules);
      elseRules = rJoin(elseRules, afterElseRules);
      return rJoin(beforeRules, rJoin(ifRules, elseRules));
    end
    // While loop recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged While {.cond, .r}: begin
      FlowFF inFF <- mkBypassFIFO;
      FlowFF outFF <- mkBypassFIFO;
      Reg#(Bool) busy[2] <- mkCReg(2, False);
      Reg#(Bool) activeStep[3] <- mkCReg(3, False);
      // XXX Note, for cond, this wire emulates the behaviour of a ConfigReg to
      // avoid scheduling issues between the inner rules updating the condition
      // and the wrapping rules reading it.
      PulseWire condValid <- mkPulseWire;
      Rules innerRules <- innerCompile(r, inFF, outFF);
      Rules wrappingRules =
      (* descending_urgency = "endStepWhile, endWhile"*)
      rules
        rule ackGoFFWhile (!busy[0]);
          //$display("%0t -- ackGoFFWhile", $time);
          goFF.deq();
          busy[0] <= True;
        endrule
        rule readCondWhile (cond);
          //$display("%0t -- readCondWhile", $time);
          condValid.send();
        endrule
        rule startStepWhile (busy[1] && condValid && !activeStep[0]);
          //$display("%0t -- startStepWhile", $time);
          inFF.enq(?);
          activeStep[0] <= True;
        endrule
        rule endStepWhile;
          //$display("%0t -- endStepWhile", $time);
          outFF.deq();
          activeStep[1] <= False;
        endrule
        rule endWhile (busy[1] && !condValid && !activeStep[2]);
          //$display("%0t -- endWhile", $time);
          busy[1] <= False;
          doneFF.enq(?);
        endrule
      endrules;
      return rJoin(innerRules, wrappingRules);
    end
  endcase
endmodule

endpackage: Recipe
