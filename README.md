# Recipe

A BSV libary providing features similar to the Stmt sub-language.

## An example

Here is what defining a Recipe looks like:
```bsv
import Recipe :: *;
import FIFO :: *;
import SpecialFIFOs :: *;

module top ();

  // define a bypass fifo to demonstrate timing properties of the Recipe
  FIFO#(Bit#(0)) nodelay <- mkBypassFIFO;
  // define some Boolean condition to use with rIfElse
  Bool cond = True;
  // define the Recipe
  Recipe r = rFastSeq(rBlock(
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
    rIfElse((cond), rAct($display("%0t -- G if", $time)), rAct($display("%0t -- G else", $time))),
    $display("%0t -- H", $time)
  ));
  // compile the Recipe to a RecipeFSM interface
  RecipeFSM m <- compile(r);

  // Start runing the recipe
  rule run; m.start(); endrule

  // On the recipe's last cyle, terminate simulation
  rule endSim (m.isLastCycle); $finish(0); endrule

endmodule
```

The expected output of the simulator generated when building this code is:
```
10 -- A
10 -- B
10 -- C (nodelay.enq)
10 -- D
10 -- E (nodelay.deq)
10 -- F
10 -- G if
10 -- H
```

## Getting started

The library sources are contained in [Recipe.bsv](Recipe.bsv). Examples
are also provided as `ExampleX.bsv` and can be built on a system with a
working installation of Bluespec by typing `make`. They can each be run
with `./exampleX`.

Like `Stmt`, `Recipe` are a straight forward way to describe state machines.
`Recipe` have the added benefit that termination can be detected with no latency,
and also allow the user to access any desired internal signal in [Recipe.bsv](Recipe.bsv).

## Library overview

The `Recipe` type is defined as a BSV union tagged:

```bsv
typedef union tagged {
// union tagged constructors...
} Recipe;
```

The BSV type constructors can be found in [Recipe.bsv](Recipe.bsv), but they are not meant to be directly used and are therefore not exported by the package. Instead, a `Recipe` is built using a combination of recipe constructors described in the [Recipe constructors](#recipe-constructors) section.

To build a state machine from a `Recipe`, the library defines a `compile` module with the folowing type signature:

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

## Recipe constructors

* The `rAct` recipe constructor simply wraps an `Action`

```bsv
function Recipe rAct(Action a);
```

* The `rActV` recipe constructor simply wraps an `ActionValue#(Bool)`

```bsv
function Recipe rActV(ActionValue#(Bool) a);
```

* The `rSeq` recipe constructor creates a sequence of `Recipe`s executed in order.
  The constructor must be used to wrap a call to `rBlock` which allows for arbitrary
  many `Recipe` to be placed in a list.

```bsv
function Recipe rSeq(List#(Recipe) rs);
```

* The `rPar` recipe constructor creates a group of `Recipe`s executed in parallel.
  The constructor must be used to wrap a call to `rBlock` which allows for arbitrary
  many `Recipe` to be placed in a list.

```bsv
function Recipe rPar(List#(Recipe) rs);
```

* The `rFastSeq` recipe constructor creates a sequence of `Recipe`s executed in order.
  If it is possible for successive `Recipes` to be scheduled in the same cycle, than
  an `rFastSeq` will execute them in the same cycle where an `rSeq` would use two cycles.
  The constructor must be used to wrap a call to `rBlock` which allows for arbitrary
  many `Recipe` to be placed in a list.

```bsv
function Recipe rFastSeq(List#(Recipe) rs);
```

* The `rIfElse` recipe constructor takes a `Bool` condition together with a pair of
  `Recipe`s, and executes the first `Recipe` if the condition is `True` or the second
  one if the condition is `False`.

```bsv
function Recipe rIfElse(Bool c, Recipe r0, Recipe r1);
```

* The `rWhile` recipe constructor takes a `Bool` condition together with a `Recipe`,
and executes the `Recipe` as long as the condition is `True`.

```bsv
function Recipe rWhile(Bool c, Recipe r);
```
