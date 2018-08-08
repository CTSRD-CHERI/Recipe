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
import Printf :: *;
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
export rAllGuard;
export rOneMatch;
export rOneMatchDelay;
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
  Action RAct;
  ActionValue#(Bool) RActV;
  Tuple4#(Module#(FlowFF), Bool, Recipe, Recipe) RIfElse;
  Tuple3#(Module#(FlowFF), Bool, Recipe) RWhile;
  Tuple3#(Module#(FlowFF), function Rules f(Rules x, Rules y), List#(Recipe)) RSeq;
  Tuple2#(Module#(FlowFF), List#(Recipe)) RPar;
  Tuple5#(Module#(FlowFF), List#(Bool), List#(Recipe), Module#(FlowFF), Recipe) ROneMatch;
} Recipe;

// Recipe block constructor (to wrap multiple recipes in a list, passed to RSeq, RPar etc..)
typeclass RecipeBlock#(type a);
  function a mkRecipe(List#(Recipe) acc);
endtypeclass

instance RecipeBlock#(List#(Recipe));
  function mkRecipe(acc) = reverse(acc);
endinstance

instance RecipeBlock#(function a f(Action elem)) provisos (RecipeBlock#(a));
  function mkRecipe(acc, elem) = mkRecipe(cons(RAct(elem), acc));
endinstance

instance RecipeBlock#(function a f(ActionValue#(Bool) elem)) provisos (RecipeBlock#(a));
  function mkRecipe(acc, elem) = mkRecipe(cons(RActV(elem), acc));
endinstance

instance RecipeBlock#(function a f(Recipe elem)) provisos (RecipeBlock#(a));
  function mkRecipe(acc, elem) = mkRecipe(cons(elem, acc));
endinstance

function a rBlock() provisos (RecipeBlock#(a));
  return mkRecipe(Nil);
endfunction

// Basic Action recipe
function Recipe rAct(Action a) = RAct(a);
// ActionValue recipe
function Recipe rActV(ActionValue#(Bool) a) = RActV(a);
// First recipe happens for a True condition, second recipe otherwise
function Recipe rIfElse(Bool c, Recipe r0, Recipe r1) = RIfElse(tuple4(mkBypassFIFO, c, r0, r1));
function Recipe rWhen(Bool c, Recipe r) = rIfElse(c, r, rAct(noAction));
// Recipe happens as long as the condition is True
function Recipe rWhile(Bool c, Recipe r) = RWhile(tuple3(mkBypassFIFO, c, r));
// All recipes happen in order
function Recipe rSeq(List#(Recipe) rs) = RSeq(tuple3(mkFIFO1, rJoinMutuallyExclusive, rs));
// XXX FastSeq:
// All recipes happen in order with no latency (separated by mkBypassFIFO).
// Data dependencies can still create latency. Any module whose firing in an early rule depends
// on the firing of a later rule (typically mkPipeLineFIFO and the likes) will cause a scheduling
// error as the rules are separated by a mkBypassFIFO, leading to a cycle in the canfire/willfire signals.
function Recipe rFastSeq(List#(Recipe) rs) = RSeq(tuple3(mkBypassFIFO, rJoinDescendingUrgency, rs));
// All recipes happen in parallel
function Recipe rPar(List#(Recipe) rs) = RPar(tuple2(mkBypassFIFO, rs));
// All recipes with predicate matching happen in parallel
function Recipe rAllGuard(List#(Bool) gs, List#(Recipe) rs) = rPar(zipWith(rWhen, gs, rs));
// First recipe with that matches happens, otherwise fall-through recipe
function Recipe rOneMatch(List#(Bool) gs, List#(Recipe) rs, Recipe r) = ROneMatch(tuple5(mkBypassFIFO, gs, rs, mkBypassFIFO, r));
function Recipe rOneMatchDelay(List#(Bool) gs, List#(Recipe) rs, Recipe r) = ROneMatch(tuple5(mkFIFO1, gs, rs, mkBypassFIFO, r));

/* TODO proprocessor macros to apply rBlock implicitly
#define rAct(a) RAct(a)
#define rActV(a) RActV(a)
#define rPar(...) RPar(rBlock(__VA_ARGS__))
#define rSeq(...) RSeq(rBlock(__VA_ARGS__))
#define rIfElse(a, b, c) RIfElse(tuple3(a, b, c))
#define rWhile(a, b) RWhile(tuple2(a, b))
...
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
  // core compiler
  //////////////////////////////////////////////////////////////////////////////
  case (r) matches
    // Action recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged RAct .a: return rules
      rule runAct;
        a;
        goFF.deq();
        doneFF.enq(?);
      endrule
    endrules;
    // ActionValue recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged RActV .av: return rules
      rule runActV;
        Bool isDone <- av;
        if (isDone) begin
          goFF.deq();
          doneFF.enq(?);
        end
      endrule
    endrules;
    // If / Else recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged RIfElse {.mkFF, .cond, .r_if, .r_else}: begin
      // declare flow control state //
      ////////////////////////////////
      FlowFF inFF_if     <- mkFF;
      FlowFF inFF_else   <- mkFF;
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
      ifRules = rJoinExecutionOrder(ifRules, afterIfRules);
      elseRules = rJoinExecutionOrder(elseRules, afterElseRules);
      return rJoinExecutionOrder(beforeRules, rJoinMutuallyExclusive(ifRules, elseRules));
    end
    // While loop recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged RWhile {.mkFF, .cond, .r}: begin
      FlowFF inFF <- mkFF;
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
    // Seq recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged RSeq {.mkFF, .rulesJoin, Nil}: return rules
      rule emptySeq; goFF.deq(); doneFF.enq(?); endrule
    endrules;
    tagged RSeq {.mkFF, .rulesJoin, .rs}: begin
      FlowFF lastFF = goFF;
      Integer seqLength = length(rs);
      List#(Recipe) rList = rs;
      // go through the list and leave the last element in there
      Rules seqRules = emptyRules;
      for (Integer i = 0; i < seqLength - 1; i = i + 1) begin
        Tuple2#(FlowFF, Rules) step <- compileWrapOut(mkFF, head(rList), lastFF);
        lastFF = tpl_1(step);
        seqRules = rulesJoin(seqRules, tpl_2(step));
        rList = tail(rList);
      end
      Rules lastStep <- innerCompile(head(rList), lastFF, doneFF);
      return rulesJoin(seqRules, lastStep);
    end
    // Par recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged RPar {.mkFF, .rs}: begin
      Integer parLength = length(rs);
      List#(FlowFF) inFFs <- replicateM(parLength, mkFF);
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
      return rJoinExecutionOrder(forkRule, rJoinExecutionOrder(branchRules, joinRule));
    end
    // OneMatch recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged ROneMatch {.mkFF, .gs, .rs, .mkDfltFF, .r}: begin
      // check for valid lengths //
      /////////////////////////////
      Integer rlen = length(rs);
      Integer glen = length(gs);
      if (rlen != glen) error(sprintf("OneMatch recipe constructor: list of guards and of recipes must be the same length (given %0d guards and %0d recipes).", glen, rlen));
      // local resources //
      /////////////////////
      PulseWire done                         <- mkPulseWireOR;
      FlowFF dfltFF                          <- mkDfltFF;
      Tuple2#(FlowFF, Rules) dflt            <- compileWrapOut(mkBypassFIFO, r, dfltFF);
      List#(FlowFF) inFFs                    <- replicateM(rlen, mkFF);
      List#(Tuple2#(FlowFF, Rules)) branches <- zipWithM (compileWrapOut(mkBypassFIFO), rs, inFFs);
      // rules for branch recipes //
      //////////////////////////////
      Rules allbranchRules = fold(rJoinMutuallyExclusive, map(tpl_2, branches));
      Rules dfltBranchRules = tpl_2(dflt);
      Rules branchRules = rJoinMutuallyExclusive(allbranchRules, dfltBranchRules);
      // rules for branch triggering //
      /////////////////////////////////
      Rules triggerRules = rules rule triggerOneMatch;
        goFF.deq();
        // trigger first matching branch, or default branch otherwise
        case(find(tpl_1, zip(gs, inFFs))) matches
          tagged Valid {.guard, .ff}: ff.enq(?);
          tagged Invalid: dfltFF.enq(?);
        endcase
      endrule endrules;
      // rules to gather each branch //
      /////////////////////////////////
      function Rules gatherRule(FIFO#(x) ff) = rules
        rule gatherOneMatch; ff.deq(); done.send(); endrule
      endrules;
      Rules allGatherRules = fold(rJoinMutuallyExclusive, map(gatherRule, map(tpl_1, branches)));
      Rules dfltGatherRules = rules
        rule gatherDfltOneMatch; tpl_1(dflt).deq(); done.send(); endrule
      endrules;
      Rules gatherRules = rJoinMutuallyExclusive(allGatherRules, dfltGatherRules);
      // rules to generate final done signal //
      /////////////////////////////////////////
      Rules finalRules = rules
        rule finalOneMatch (done); doneFF.enq(?); endrule
      endrules;
      // compose all rules and return
      return rJoinExecutionOrder(triggerRules, rJoinExecutionOrder(branchRules, rJoinExecutionOrder(gatherRules, finalRules)));
    end
  endcase
endmodule

endpackage
