# Recipe

A BSV library providing a way to easily build state machines, similarly to the `Stmt` BSV sub-language.

## An example

Here is a BSV code sample showing how to define Recipe:

```bsv
import Recipe :: *;
#include "RecipeMacros.h"

import FIFO :: *;

module top ();

  FIFO#(Bit#(0)) delay <- mkFIFO1;

  Recipe r = FastSeq
    $display("%0t -- A", $time),
    $display("%0t -- B", $time),
    action
      $display("%0t -- C (delay.enq)", $time);
      delay.enq(?);
    endaction,
    $display("%0t -- D", $time),
    $display("%0t -- E", $time),
    action
      $display("%0t -- F (delay.deq)", $time);
      delay.deq;
    endaction,
    $display("%0t -- G", $time),
    $display("%0t -- H", $time)
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
```

The expected output of the simulator generated when building this code is:
```
starting at time 10
------------------------------------------
10 -- A
10 -- B
10 -- C (delay.enq)
10 -- D
10 -- E
20 -- F (delay.deq)
20 -- G
20 -- H
------------------------------------------
finishing at time 20
```

## Getting started

The library sources are contained in [Recipe.bsv](Recipe.bsv). On top of the standard BSV libraries, Recipe relies on the [BlueBasics](https://github.com/CTSRD-CHERI/BlueBasics.git) library, which is a git submodule of this git repo. It must be checked out before building Recipe, by running the following:

```sh
$ git submodule update --init --recursive
```

Some Recipe examples are provided in the [examples](examples) directory. On a system with a working BSV setup, the examples can be built by typing `make`. Each example can be executed by running the generated script (`simExample*`) in the `output` directory.

`Recipe` have the added benefit over the standard BSV `Stmt` type that termination can be detected with no latency, and also allow the user to access any desired internal signal in [Recipe.bsv](Recipe.bsv). Some additional more advanced Recipe constructors can be easily created, such as `rOneMatch` or `rMutExGroup`...

## Library overview

The `Recipe` type is defined as a BSV union tagged:

```bsv
typedef union tagged {
// union tagged constructors...
} Recipe;
```

The BSV type constructors can be found in [Recipe.bsv](Recipe.bsv), but they are not meant to be directly used and are therefore not exported by the package. Instead, a `Recipe` is built using a combination of recipe constructors (described in the [Recipe constructors](#recipe-constructors) section).

To build a state machine from a `Recipe`, the library defines a `compile` module with the following type signature:

```bsv
module [Module] compile#(Recipe r) (RecipeFSM);
```

A `Recipe` can be compiled using the `compile` module to get a `RecipeFSM` interface as follows:

```bsv
Recipe r;
// create Recipe ...
RecipeFSM m <- compile(r);
```

The `RecipeFSM` interface is defined as follows:

```bsv
interface RecipeFSM;
  method Action start();
  method Bool isLastCycle();
  method Bool isDone();
  method Action waitForDone();
endinterface
```

As their names suggest,
* the `start` method starts the machine
* the `isLastCycle` method returns `True` if the cycle is the last one before the machine is done
* the `isDone` method returns `True` if the machine is done
* the `waitForDone` method can be used to add an implicit condition to wait for the machine to be done

Additionally, the library provides the `compileRules` module with a type signature as follows:

```bsv
module [Module] compileRules#(Recipe r) (Tuple2#(Rules, RecipeFSM));
```

As opposed to the `compile` module, the `compileRules` module will return the compiled `Rules` together with the `RecipeFSM` interface rather than adding the rules to the current module, giving the user the ability to schedule the generated rules
according to their needs.

The `compileMutuallyExclusive` module compiles a list of recipes, adds the rules for each recipe such
that they are mutually exclusive, and returns a list of interfaces to the generated machines.

```bsv
module [Module] compileMutuallyExclusive#(List#(Recipe) rs) (List#(RecipeFSM));
```

Another way to control mutual exclusivity of the generated rules is to use the `rMutExGroup` recipe constructor.

## A readable `Recipe`

To help define `Recipe`s in a concise and readable way, macros are provided that simplify the calls to common recipe constructors:

```bsv
Seq
  $display("A"),
  $display("B"),
  $display("C")
End
```

is equivalent to

```bsv
rSeq(rBlock(
  $display("A"),
  $display("B"),
  $display("C")
))
```

Similarly, macros are defined for `Par`, `FastSeq`, `If` and `Else`, `When` and `While`. Those are defined in [RecipeMacros.h](RecipeMacros.h), which must be included in your BSV sources as follows:

```bsv
#include "RecipeMacros.h"
```

Additionally, the call to the BSV compiler should have the `-cpp` flag together with `-Xcpp -Ipath/to/Recipe` where `path/to/Recipe` is the path to the folder containing [RecipeMacros.h](RecipeMacros.h).

Alternatively, the [RecipeMacros.inc](RecipeMacros.inc) file defines the `` `Seq ``, `` `Par ``, `` `FastSeq ``, `` `If ``, `` `Else ``, `` `When `` and `` `While `` verilog preprocessor macros, and do not require any additional flag on the BSC compiler invocation.

## Recipe constructors

* The `rAct` recipe constructor simply wraps an `Action`

```bsv
function Recipe rAct(Action a);
```

* The `rActV` recipe constructor simply wraps an `ActionValue#(Bool)`

```bsv
function Recipe rActV(ActionValue#(Bool) a);
```

* The `rSeq` recipe constructor creates a sequence of `Recipe`s executed in order, with one cycle of latency between each of them. The constructor expects a `List#(Recipe)` and should be used to wrap a call to `rBlock` which allows for arbitrary many `Recipe` to be placed in a list.

```bsv
function Recipe rSeq(List#(Recipe) rs);
```

* The `rPar` recipe constructor creates a group of `Recipe`s executed in parallel. The constructor expects a `List#(Recipe)` and should be used to wrap a call to `rBlock` which allows for arbitrary many `Recipe` to be placed in a list.

```bsv
function Recipe rPar(List#(Recipe) rs);
```

* The `rFastSeq` recipe constructor creates a sequence of `Recipe`s executed in order with no latency when possible. The constructor expects a `List#(Recipe)` and should be used to wrap a call to `rBlock` which allows for arbitrary many `Recipe` to be placed in a list.

```bsv
function Recipe rFastSeq(List#(Recipe) rs);
```

* The `rIfElse` recipe constructor takes a `Bool` condition and two `Recipe`s. It executes the first `Recipe` if the condition is `True` and the second `Recipe` if the condition is `False`.

```bsv
function Recipe rIfElse(Bool c, Recipe r0, Recipe r1);
```

* The `rWhen` recipe constructor takes a `Bool` condition and a `Recipe`s. It executes the `Recipe` if the condition is `True`.

```bsv
function Recipe rWhen(Bool c, Recipe r);
```

* The `rWhile` recipe constructor takes a `Bool` condition together with a `Recipe`. It executes the `Recipe` as long as the condition is `True`.

```bsv
function Recipe rWhile(Bool c, Recipe r);
```

* The `rMutExGroup` recipe constructor takes a mutual exclusion group name as a `String` and a `Recipe` to be added to that mutual exclusion group. When compiling, `Recipe`s belonging to a mutual exclusion group generate rules that will be scheduled as mutually exclusive to those belonging to a different mutual exclusion group. `Recipe`s that are not tagged with any mutual exclusion group are scheduled normally.

```bsv
function Recipe rMutExGroup(String n, Recipe r);
```

* The `rAllGuard` recipe constructor takes a list of guards `List#(Bool)`, and a list of recipes `List#(Recipe)`. It executes all recipes with a True guard in parallel.

```bsv
function Recipe rAllGuard(List#(Bool) gs, List#(Recipe) rs)
```

* The `rOneMatch` recipe constructor takes a list of guards `List#(Bool)`, a list of recipes `List#(Recipe)`, and an extra "fall-through" recipe. The first recipe in the list order with a corresponding guard set to True is executed. If no recipe matches, the "fall-through" recipe is executed.

```bsv
function Recipe rOneMatch(List#(Bool) gs, List#(Recipe) rs, Recipe r)
```
