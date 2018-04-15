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

The `Recipe` type is provided to hold the recipe being built. It is defined as follows:

```bsv
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
```

A `Recipe` is built using a combination of recipe constructors such as `rSeq` to create a sequence for example.
Recipe constructors are described in the [Recipe constructors](#recipe-constructors) section. The library
defines a `compile` module:

```bsv
module [Module] compile#(Recipe r) (RecipeFSM);
  Tuple2#(Rules, RecipeFSM) x <- topCompile(r);
  addRules(tpl_1(x));
  return tpl_2(x);
endmodule
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

As per their names suggest,
* the `start` method starts the machine
* the `isLastCycle` method returns `True` if the cycle is the last one before the machine is done
* the `isDone` method returns `True` if the machine is done
* the `waitForDone` method can be used to add an implicit condition to wait for the machine to be done

Additionally, the library provides the `compileRules` module that returns the `Rules` handle rather
than performing the call to `addRules`, giving the user the ability to schedule the generated rules
according to their needs.

```bsv
module [Module] compileRules#(Recipe r) (Tuple2#(Rules, RecipeFSM));
  Tuple2#(Rules, RecipeFSM) x <- topCompile(r);
  return x;
endmodule
```

The `compileMutuallyExclusive` module compiles a list of recipes, adds the rules for each recipe such
that they are mutually exclusive, and returns a list of interfaces to the generated machines.

```bsv
module [Module] compileMutuallyExclusive#(List#(Recipe) rs) (List#(RecipeFSM));
  List#(Tuple2#(Rules, RecipeFSM)) xs <- mapM(topCompile, rs);
  addRules(fold(rJoinMutuallyExclusive, map(tpl_1, xs)));
  return map(tpl_2,xs);
endmodule
```

## Recipe constructors

* The `rAct` recipe constructor simply wraps an `Action`

```bsv
function Recipe rAct(Action a) = Act(a);
```

* The `rActV` recipe constructor simply wraps an `ActionValue#(Bool)`

```bsv
function Recipe rActV(ActionValue#(Bool) a) = ActV(a);
```

* The `rSeq` recipe constructor creates a sequence of `Recipe`s executed in order.
  The constructor must be used to wrap a call to `rBlock` which allows for arbitrary
  many `Recipe` to be placed in a list.

```bsv
function Recipe rSeq(List#(Recipe) rs) = Seq(rs);
```

* The `rPar` recipe constructor creates a group of `Recipe`s executed in parallel.
  The constructor must be used to wrap a call to `rBlock` which allows for arbitrary
  many `Recipe` to be placed in a list.

```bsv
function Recipe rPar(List#(Recipe) rs) = Par(rs);
```

* The `rFastSeq` recipe constructor creates a sequence of `Recipe`s executed in order.
  If it is possible for successive `Recipes` to be scheduled in the same cycle, than
  an `rFastSeq` will execute them in the same cycle where an `rSeq` would use two cycles.
  The constructor must be used to wrap a call to `rBlock` which allows for arbitrary
  many `Recipe` to be placed in a list.

```bsv
function Recipe rFastSeq(List#(Recipe) rs) = FastSeq(rs);
```

* The `rIfElse` recipe constructor takes a `Bool` condition together with a pair of
  `Recipe`s, and executes the first `Recipe` if the condition is `True` or the second
  one if the condition is `False`.

```bsv
function Recipe rIfElse(Bool c, Recipe r0, Recipe r1) = IfElse(tuple3(c, r0, r1));
```

* The `rWhile` recipe constructor takes a `Bool` condition together with a `Recipe`,
and executes the `Recipe` as long as the condition is `True`.

```bsv
function Recipe rWhile(Bool c, Recipe r) = While(tuple2(c, r));
```
