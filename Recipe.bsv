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
import FIFO :: *;
import FIFOF :: *;
import GetPut :: *;
import Connectable :: * ;
import SpecialFIFOs :: *;

// non standard packages
import Monoid :: *;
import ListExtra :: *;
import Dict :: *;
import MasterSlave :: *;
import SourceSink :: *;

// list of exported indentifiers
export Recipe;
export rAct;
export rActV;
export rFastSeq;
export rSeq;
export rPar;
export rAllGuard;
export rOneMatch;
export rIfElse;
export rWhen;
export rWhile;
export rMutExGroup;
export rBlock;
export RecipeBlock;
export ToRecipe(..);
export RecipeFSM(..);
export mkRecipeFSMSlave;
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
  Tuple2#(String, Recipe) RMutexGroup;
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

// Class for converting types to a Recipe
typeclass ToRecipe#(type a);
  function Recipe toRecipe(a x);
endtypeclass

// Lift Action to Recipe
instance ToRecipe#(Action);
  function Recipe toRecipe(Action x) = rAct(x);
endinstance

// Lift Recipe to Recipe
instance ToRecipe#(Recipe);
  function Recipe toRecipe(Recipe x) = x;
endinstance

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
// Add recipe to a mutex group
function Recipe rMutExGroup(String s, Recipe r) = RMutexGroup(tuple2(s, r));

// Recipe interfaces
////////////////////////////////////////////////////////////////////////////////

interface RecipeFSM;
  method Bool canStart();
  method Action start();
  method Bool isLastCycle();
  method Bool isDone();
  method Action waitForDone();
endinterface

module [Module] mkRecipeFSMSlave#(
  function Recipe recipe_func (in_t args, Sink#(out_t) outsnk))
  (Slave#(in_t, out_t)) provisos (Bits#(in_t, in_sz), Bits#(out_t, out_sz));
  Reg#(in_t) arg_reg[2] <- mkCRegU(2);
  FIFOF#(out_t) ret_ff <- mkBypassFIFOF;
  RecipeFSM recipe_fsm <- compile(recipe_func(arg_reg[1], toSink(ret_ff)));
  interface Sink sink;
    method canPut = recipe_fsm.canStart;
    method put(x) if (recipe_fsm.canStart) = action
      arg_reg[0] <= x;
      recipe_fsm.start;
    endaction;
  endinterface
  interface source = toSource(ret_ff);
endmodule

// Recipe compiler front modules
////////////////////////////////////////////////////////////////////////////////

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

`define DICT_T Dict#(Maybe#(String), Rules)

Maybe#(String) noMutEx = tagged Invalid;

// top compile module wrapping the machine with appropriate flow control
module [Module] topCompile#(Recipe r) (Tuple2#(Rules, RecipeFSM));
  FlowFF goFF <- mkBypassFIFO;
  FIFOF#(Bit#(0)) busyFF <- mkBypassFIFOF; // To push back while the machine is busy
  FlowFF doneFF <- mkBypassFIFO;
  Reg#(Bool) doneReg[3] <- mkCReg(3, False);
  PulseWire lastCycle <- mkPulseWire;
  // compile recipe and gather rules
  `DICT_T compiledRules <- innerCompile(noMutEx, r, goFF, doneFF);
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
    method Bool canStart() = busyFF.notFull();
    method Bool isDone() = doneReg[2];
    method Action waitForDone() if (doneReg[2]) = action endaction;
  endmodule
  RecipeFSM machine <- mkIFC;
  // compose the rules from the dictionary, respecting mutual exclusivity groups
  function isNormal(x) = !isValid(tpl_1(x));
  function  isMutEx(x) = isValid(tpl_1(x));
  `DICT_T normalDict = Dict::filter(isNormal, compiledRules);
  `DICT_T  mutExDict = Dict::filter(isMutEx, compiledRules);

  Rules normalRules = foldl(rJoin, emptyRules, values(normalDict));
  Rules  mutExRules = foldl(rJoinMutuallyExclusive, emptyRules, values(mutExDict));
  return tuple2(rJoin(wrappingRules, rJoin(normalRules, mutExRules)), machine);
endmodule

// main compile module
module [Module] innerCompile#(Maybe#(String) mutex_tag, Recipe r, FlowFF goFF, FlowFF doneFF) (`DICT_T);

  // module helper wrapping Recipe output with a provided FlowFF constructor
  //////////////////////////////////////////////////////////////////////////////
  module [Module] compileWrapOut#(Module#(FlowFF) mkFF, Maybe#(String) t, Recipe r, FlowFF inFF) (Tuple2#(FlowFF, `DICT_T));
    FlowFF outFF <- mkFF;
    `DICT_T innerRules <- innerCompile(t, r, inFF, outFF);
    return tuple2(outFF, innerRules);
  endmodule
  // core compiler
  //////////////////////////////////////////////////////////////////////////////
  case (r) matches
    // Action recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged RAct .a: return dict(mutex_tag, rules
      rule runAct;
        a;
        goFF.deq();
        doneFF.enq(?);
      endrule
    endrules);
    // ActionValue recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged RActV .av: return dict(mutex_tag, rules
      rule runActV;
        Bool isDone <- av;
        if (isDone) begin
          goFF.deq();
          doneFF.enq(?);
        end
      endrule
    endrules);
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
      `DICT_T ifRules   <- innerCompile(mutex_tag, r_if, inFF_if, outFF_if);
      `DICT_T elseRules <- innerCompile(mutex_tag, r_else, inFF_else, outFF_else);
      `DICT_T beforeRules = dict(noMutEx, rules rule enqCondFF;
        goFF.deq();
        condFF.enq(cond);
        if (cond) inFF_if.enq(?);
        else inFF_else.enq(?);
      endrule endrules);
      `DICT_T afterIfRules = dict(noMutEx, rules rule finishIf(condFF.first);
        condFF.deq();
        outFF_if.deq();
        doneFF.enq(?);
      endrule endrules);
      `DICT_T afterElseRules = dict(noMutEx, rules rule finishElse(!condFF.first);
        condFF.deq();
        outFF_else.deq();
        doneFF.enq(?);
      endrule endrules);
      // join rules //
      ////////////////
      ifRules = mergeWith(rJoinExecutionOrder, ifRules, afterIfRules);
      elseRules = mergeWith(rJoinExecutionOrder, elseRules, afterElseRules);
      `DICT_T ifElseRules = mergeWith(rJoinMutuallyExclusive, ifRules, elseRules);
      return mergeWith(rJoinExecutionOrder, beforeRules, ifElseRules);
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
      `DICT_T innerRules <- innerCompile(mutex_tag, r, inFF, outFF);
      `DICT_T wrappingRules = dict(noMutEx,
      (* descending_urgency = "endStepWhile, endWhile"*)
      rules
        rule ackGoFFWhile (!busy[0]);
          //$display("%0t -- ackGoFFWhile", $time);
          goFF.deq();
          busy[0] <= True;
        endrule
        rule readCondWhile (cond && busy[1]);
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
      endrules);
      return mconcat(list(innerRules, wrappingRules));
    end
    // Seq recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged RSeq {.mkFF, .rulesJoin, Nil}: return dict(noMutEx, rules
      rule emptySeq; goFF.deq(); doneFF.enq(?); endrule
    endrules);
    tagged RSeq {.mkFF, .rulesJoin, .rs}: begin
      FlowFF lastFF = goFF;
      Integer seqLength = length(rs);
      List#(Recipe) rList = rs;
      // go through the list and leave the last element in there
      `DICT_T seqRules = mempty;
      for (Integer i = 0; i < seqLength - 1; i = i + 1) begin
        Tuple2#(FlowFF, `DICT_T) step <- compileWrapOut(mkFF, mutex_tag, head(rList), lastFF);
        lastFF = tpl_1(step);
        seqRules = mergeWith(rulesJoin, seqRules, tpl_2(step));
        rList = tail(rList);
      end
      `DICT_T lastStep <- innerCompile(mutex_tag, head(rList), lastFF, doneFF);
      return mergeWith(rulesJoin, seqRules, lastStep);
    end
    // Par recipe construct
    ////////////////////////////////////////////////////////////////////////////
    tagged RPar {.mkFF, .rs}: begin
      Integer parLength = length(rs);
      List#(FlowFF) inFFs <- replicateM(parLength, mkFF);
      List#(Tuple2#(FlowFF, `DICT_T)) branches <- zipWithM (compileWrapOut(mkBypassFIFO, mutex_tag), rs, inFFs);
      `DICT_T branchRules = mconcat(map(tpl_2, branches));
      `DICT_T forkRule = dict(noMutEx, rules rule forkPar;
        goFF.deq();
        function Action doEnq(FIFO#(x) ff) = action ff.enq(?); endaction;
        joinActions(map(doEnq, inFFs));
      endrule endrules);
      `DICT_T joinRule = dict(noMutEx, rules rule joinPar;
        function Action doDeq(FIFO#(x) ff) = action ff.deq(); endaction;
        joinActions(map(doDeq, map(tpl_1, branches)));
        doneFF.enq(?);
      endrule endrules);
      return concatWith(rJoinExecutionOrder, list(forkRule, branchRules, joinRule));
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
      PulseWire done                           <- mkPulseWireOR;
      FlowFF dfltFF                            <- mkDfltFF;
      Tuple2#(FlowFF, `DICT_T) dflt            <- compileWrapOut(mkBypassFIFO, mutex_tag, r, dfltFF);
      List#(FlowFF) inFFs                      <- replicateM(rlen, mkFF);
      List#(Tuple2#(FlowFF, `DICT_T)) branches <- zipWithM (compileWrapOut(mkBypassFIFO, mutex_tag), rs, inFFs);
      // rules for branch recipes //
      //////////////////////////////
      `DICT_T allbranchRules = concatWith(rJoinMutuallyExclusive, map(tpl_2, branches));
      `DICT_T dfltBranchRules = tpl_2(dflt);
      `DICT_T branchRules = mergeWith(rJoinMutuallyExclusive, allbranchRules, dfltBranchRules);
      // rules for branch triggering //
      /////////////////////////////////
      `DICT_T triggerRules = dict(noMutEx, rules rule triggerOneMatch;
        goFF.deq();
        // trigger first matching branch, or default branch otherwise
        case(find(tpl_1, zip(gs, inFFs))) matches
          tagged Valid {.guard, .ff}: ff.enq(?);
          tagged Invalid: dfltFF.enq(?);
        endcase
      endrule endrules);
      // rules to gather each branch //
      /////////////////////////////////
      function Rules gatherRule(FIFO#(x) ff) = rules
        rule gatherOneMatch; ff.deq(); done.send(); endrule
      endrules;
      Rules allGatherRules = fold(rJoinMutuallyExclusive, map(gatherRule, map(tpl_1, branches)));
      Rules dfltGatherRules = rules
        rule gatherDfltOneMatch; tpl_1(dflt).deq(); done.send(); endrule
      endrules;
      `DICT_T gatherRules = dict(noMutEx, rJoinMutuallyExclusive(allGatherRules, dfltGatherRules));
      // rules to generate final done signal //
      /////////////////////////////////////////
      `DICT_T finalRules = dict(noMutEx, rules
        rule finalOneMatch (done); doneFF.enq(?); endrule
      endrules);
      // compose all rules and return
      return concatWith(rJoinExecutionOrder, list(triggerRules, branchRules, gatherRules, finalRules));
    end
    // Tag recipe
    ////////////////////////////////////////////////////////////////////////////
    tagged RMutexGroup {.mutex_str, .recipe}: begin
      let d <- innerCompile(Valid(mutex_str), recipe, goFF, doneFF);
      return d;
    end
  endcase
endmodule

`undef DICT_T

endpackage
