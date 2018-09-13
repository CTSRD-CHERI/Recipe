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

import Recipe :: *;
import FIFOF :: *;
import MasterSlave :: *;
import SourceSink :: *;
#include "RecipeMacros.h"

module top ();

  Reg#(Bit#(8)) cnt <- mkReg(0);
  function Recipe multiCycleAdder (
    Tuple2#(Bit#(32), Bit#(32)) args,
    FIFOF#(Bit#(32)) sumff);
    match {.a, .b} = args;
    return Seq
      $display("%0t -- a (%0d) + b (%0d)", $time, a, b),
      writeReg(cnt, 0),
      While (cnt < 10) writeReg(cnt, cnt + 1) End,
      sumff.enq(a + b)
    End;
  endfunction

  let adder <- mkRecipeFSMSlave(multiCycleAdder);

  Recipe r = Seq
    adder.sink.put(tuple2(10, 15)),
    action
      let x <- adder.source.get;
      $display("%0t -- a + b = %0d", $time, x);
    endaction,
    adder.sink.put(tuple2(856, 994)),
    action
      let x <- adder.source.get;
      $display("%0t -- a + b = %0d", $time, x);
    endaction,
    adder.sink.put(tuple2(42, 3)),
    action
      let x <- adder.source.get;
      $display("%0t -- a + b = %0d", $time, x);
    endaction
  End;
  RecipeFSM m <- compile(r);

  // Start runing the recipe
  rule run;
    $display("starting at time %0t", $time);
    $display("------------------------------------------");
    m.start();
  endrule

  // On the recipe's last cyle, terminate simulation
  rule endSim (m.isLastCycle);
    $display("------------------------------------------");
    $display("finishing at time %0t", $time);
    $finish(0);
  endrule

endmodule
