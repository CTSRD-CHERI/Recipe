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

import FIFO :: *;
import SpecialFIFOs :: *;
import List :: *;

import Recipe :: *;
#include "RecipeMacros.h"

// Example use of Recipe
module top ();

  // Defining some counters and fifos
  Reg#(UInt#(8)) cnt <- mkReg(0);
  Reg#(UInt#(8)) runningCnt <- mkReg(0);
  rule everyCycle;
    runningCnt <= runningCnt + 1;
  endrule
  //FIFO#(Bit#(0)) delay <- mkPipelineFIFO;
  FIFO#(Bit#(0)) delay <- mkFIFO1;
  FIFO#(Bit#(0)) nodelay <- mkBypassFIFO;

  // Defining various test recipes
  Recipe r0 = While (cnt < 50)
    action
      $display("%0t -- delaying, cnt = %0d", $time, cnt);
      cnt <= cnt + 1;
    endaction
  End;
  Recipe r1 = Seq
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    $display("%0t -- C", $time),
    $display("%0t -- D", $time),
    FastSeq
      $display("%0t -- E", $time),
      $display("%0t -- F", $time),
      $display("%0t -- G", $time),
      $display("%0t -- H", $time)
    End
  End;
  Recipe r2 = Seq
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    $display("%0t -- C", $time),
    $display("%0t -- D", $time),
    $display("%0t -- E", $time),
    $display("%0t -- F", $time),
    If (cnt >= 50)
      $display("%0t -- G if", $time)
    Else
      Seq
        $display("%0t -- G else 1", $time),
        $display("%0t -- G else 2", $time)
      End
    End,
    $display("%0t -- H", $time)
  End;
  Recipe r3 = Par
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    $display("%0t -- C", $time),
    $display("%0t -- D", $time),
    $display("%0t -- E", $time),
    $display("%0t -- F", $time),
    $display("%0t -- G", $time),
    $display("%0t -- H", $time)
  End;
  Recipe r4 = Par
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    While (cnt < 50)
      action
        $display("%0t -- delaying, cnt = %0d", $time, cnt);
        cnt <= cnt + 1;
      endaction
    End,
    $display("%0t -- C", $time)
  End;
  Recipe r5 = Seq
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    While (cnt < 50)
      action
        $display("%0t -- delaying, cnt = %0d", $time, cnt);
        cnt <= cnt + 1;
      endaction
    End,
    $display("%0t -- C", $time)
  End;
  Recipe r6 = While (runningCnt < 50)
    Par
      $display("%t -- A, runningCnt = %0d", $time, runningCnt),
      $display("%t -- B, runningCnt = %0d", $time, runningCnt)
    End
  End;
  Recipe r7 = While (cnt < 50)
    Par
      $display("%t -- A, runningCnt = %0d", $time, runningCnt),
      $display("%t -- B, runningCnt = %0d", $time, runningCnt),
      action cnt <= cnt + 1; endaction
    End
  End;
  Recipe r8 = Seq
    Par
      $display("1a %t", $time),
      $display("1b %t", $time)
    End,
    $display("2 %t", $time),
    While (cnt < 50)
      Par
        $display("delaying %t", $time),
        action cnt <= cnt + 1; endaction
      End
    End,
    $display("3 %t", $time)
  End;
  Recipe r9 = Seq
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    action
      $display("%0t -- C (delay.enq)", $time);
      delay.enq(?);
    endaction,
    $display("%0t -- D", $time),
    action
      $display("%0t -- E (delay.deq)", $time);
      delay.deq();
    endaction,
    $display("%0t -- F", $time),
    If (cnt < 50)
      $display("%0t -- G if", $time)
    Else
      $display("%0t -- G else", $time)
    End,
    $display("%0t -- H", $time)
  End;
  Recipe r10 = Par
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    action
      $display("%0t -- C (delay.enq)", $time);
      delay.enq(?);
    endaction,
    $display("%0t -- D", $time),
    action
      $display("%0t -- E (delay.deq)", $time);
      delay.deq();
    endaction,
    $display("%0t -- F", $time),
    If (cnt < 50)
      $display("%0t -- G if", $time)
    Else
      $display("%0t -- G else", $time)
    End,
    $display("%0t -- H", $time)
  End;
  Recipe r11 = rFastSeq(rBlock(
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    rAct(action
      $display("%0t -- C (delay.enq)", $time);
      delay.enq(?);
    endaction),
    $display("%0t -- D", $time),
    rAct(action
      $display("%0t -- E (delay.deq)", $time);
      delay.deq();
    endaction),
    $display("%0t -- F", $time),
    rIfElse((cnt >= 50),
      rAct($display("%0t -- G if", $time)),
      rFastSeq(rBlock(
        rAct($display("%0t -- G else 1", $time)),
        rAct($display("%0t -- G else 2", $time))
      ))
    ),
    $display("%0t -- H", $time)
  ));
  Recipe r12 = rSeq(rBlock(
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    rAct(action
      $display("%0t -- C (nodelay.enq)", $time);
      nodelay.enq(?);
    endaction),
    $display("%0t -- D", $time),
    rAct(action
      $display("%0t -- E (nodelay.deq)", $time);
      nodelay.deq();
    endaction),
    $display("%0t -- F", $time),
    rIfElse((cnt < 50), rAct($display("%0t -- G if", $time)), rAct($display("%0t -- G else", $time))),
    $display("%0t -- H", $time)
  ));
  Recipe r13 = rPar(rBlock(
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    rAct(action
      $display("%0t -- C (nodelay.enq)", $time);
      nodelay.enq(?);
    endaction),
    $display("%0t -- D", $time),
    rAct(action
      $display("%0t -- E (nodelay.deq)", $time);
      nodelay.deq();
    endaction),
    $display("%0t -- F", $time),
    rIfElse((cnt < 50), rAct($display("%0t -- G if", $time)), rAct($display("%0t -- G else", $time))),
    $display("%0t -- H", $time)
  ));
  Recipe r14 = rFastSeq(rBlock(
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    rAct(action
      $display("%0t -- C (nodelay.enq)", $time);
      nodelay.enq(?);
    endaction),
    $display("%0t -- D", $time),
    rAct(action
      $display("%0t -- E (nodelay.deq)", $time);
      nodelay.deq();
    endaction),
    $display("%0t -- F", $time),
    rIfElse((cnt < 50), rAct($display("%0t -- G if", $time)), rAct($display("%0t -- G else", $time))),
    $display("%0t -- H", $time)
  ));
  Recipe r15 = rWhile((cnt < 10), rPar(rBlock(
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    rAct(action
      $display("%0t -- C (enq)", $time);
      delay.enq(?);
    endaction),
    $display("%0t -- D", $time),
    rAct(action
      $display("%0t -- E (deq)", $time);
      delay.deq();
    endaction),
    $display("%0t -- F", $time),
    rAct(action cnt <= cnt + 1; endaction)
  )));
  Recipe r16 = rFastSeq(rBlock(
    $display("%0t -- A", $time),
    $display("%0t -- B", $time)
  ));
  Recipe r17 = rFastSeq(rBlock(
    action
      $display("%0t -- A (enq)", $time);
      delay.enq(?);
    endaction,
    action
      $display("%0t -- B (deq)", $time);
      delay.deq();
    endaction
  ));
  Recipe r18 = rWhile((cnt < 2), rPar(rBlock(
    $display("%0t -- A", $time),
    rAct(action
      $display("%0t -- B (enq)", $time);
      delay.enq(?);
    endaction),
    $display("%0t -- C", $time),
    rAct(action
      $display("%0t -- D (deq)", $time);
      delay.deq();
    endaction),
    $display("%0t -- E", $time),
    rAct(action cnt <= cnt + 1; endaction)
  )));
  Recipe r19 = rWhile((cnt < 10), rFastSeq(rBlock(rAllGuard(
  // list of 3 bools
  cons(False,
  cons(True,
  cons(True,
  Nil))),
  // list of 3 recipes
  cons(rAct(action $display("%0t -- A", $time); endaction),
  cons(rAct(action $display("%0t -- B", $time); endaction),
  cons(rSeq(rBlock(
    action $display("%0t -- C (1)", $time); endaction,
    action $display("%0t -- C (2)", $time); endaction,
    action $display("%0t -- C (3)", $time); endaction
    )),
  Nil)))
  ),
  action cnt <= cnt + 1; $display("%0t -- iteration %0d done", $time, cnt); endaction
  )));
  Recipe r20 = rWhile((cnt < 10), rFastSeq(rBlock(rOneMatch(
  // list of 3 bools
  cons(False,
  cons(True,
  cons(True,
  Nil))),
  // list of 3 recipes
  cons(rAct(action $display("%0t -- A", $time); endaction),
  cons(rAct(action $display("%0t -- B", $time); endaction),
  cons(rSeq(rBlock(
    action $display("%0t -- C (1)", $time); endaction,
    action $display("%0t -- C (2)", $time); endaction,
    action $display("%0t -- C (3)", $time); endaction
    )),
  Nil))),
  rAct(action $display("%0t -- No recipe matched...", $time); endaction)),
  action cnt <= cnt + 1; $display("%0t -- iteration %0d done", $time, cnt); endaction
  )));
  Reg#(Bool) isReset <- mkReg(True);
  Reg#(Bit#(6)) doTheCount <- mkReg(0);
  Recipe r21 = rIfElse(isReset,
    rAct(action isReset <= False; $display("%0t -- done reset", $time); endaction),
    rWhile(doTheCount < 32, rSeq(rBlock(
      action $display("%0t -- will do the count soon...", $time); endaction,
      action doTheCount <= doTheCount + 1; $display("%0t -- counted up from %0d...", $time, doTheCount); endaction
    )))
  );
  Recipe r22 = rSeq(rBlock(
    rAct(action
      $display("%0t -- first step ...", $time);
      // clear reset after first cycle
      isReset <= False;
    endaction),
    rWhile(True, rFastSeq(rBlock(
      action $display("%0t -- inner step A", $time); endaction,
      action $display("%0t -- inner step B", $time); endaction
    )))
  ));
  Recipe r23 = rSeq(rBlock(
    rAct(action
      $display("%0t -- first step ...", $time);
      // clear reset after first cycle
      isReset <= False;
    endaction),
    rFastSeq(rBlock(
      action $display("%0t -- inner step A", $time); endaction,
      action $display("%0t -- inner step B", $time); endaction
    ))
  ));
  Recipe r24 = rWhile(True, rFastSeq(rBlock(
  action $display("%0t -- pre check", $time); endaction,
  rOneMatch(
    // list of 3 bools
    cons(False,
    cons(False,
    cons(True,
    Nil))),
    // list of 3 recipes
    map(rMutExGroup("otherGroup"), cons(rAct(action $display("%0t -- A", $time); endaction),
    cons(rAct(action $display("%0t -- B", $time); endaction),
    cons(rSeq(rBlock(
      action $display("%0t -- C (1)", $time); endaction,
      action $display("%0t -- C (2)", $time); endaction,
      action $display("%0t -- C (3)", $time); endaction
      )),
    Nil)))),
    // fallback recipe
    rAct(action $display("%0t -- No recipe matched...", $time); endaction)),
  action $display("%0t -- post check", $time); endaction
  )));

  // Compile one of the recipes
  //let m <- compile(r0);
  //let m <- compile(r1);
  //let m <- compile(r2);
  //let m <- compile(r3);
  //let m <- compile(r4);
  //let m <- compile(r5);
  //let m <- compile(r6);
  //let m <- compile(r7);
  //let m <- compile(r8);
  //let m <- compile(r9);
  //let m <- compile(r10);
  //let m <- compile(r11);
  //let m <- compile(r12);
  //let m <- compile(r13);
  //let m <- compile(r14);
  //let m <- compile(r15);
  //let m <- compile(r16);
  //let m <- compile(r17);
  //let m <- compile(r18);
  //let m <- compile(r19);
  //let m <- compile(r20);
  //let m <- compile(r21);
  //let m <- compile(r22);
  //let m <- compile(r23);
  let m <- compile(r24);

  // Start runing the recipe
  rule run; m.start(); endrule

  // On the recipe's last cyle, terminate simulation
  rule endSim (m.isLastCycle); $finish(0); endrule

endmodule
