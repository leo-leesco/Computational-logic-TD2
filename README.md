# SAT solver

## Compiling

Simply run :
```shell
ocamlopt sat.ml && ./a.out
```
This yields the best performance as heavy computations are done for SAT solving (and not all optimizations are implemented).

## Testing on `.cnf` formatted files

[Specification of `.cnf`](https://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html)

You can test whether your formatted CNF is satisfiable or not by adding a test case (around line 264) :
```ocaml
assert (dpll_pure_unit (parse "<file>"));
```

## Testing whether a sudoku has a solution (not necessarily unique)

Add a new test case (at the end of the file) in the form of a 2D-array `[| [| … |] , … , [| … |] |]` with clues ranging `0-8` and blanks marked as `9`.

## TODO

Finish implementing the DPLL optimizations.
