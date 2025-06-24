open TD2.Dpll
open TD2.Sudoku

let () =
  let rec print_tuples = function
    | [] -> ()
    | (a, b) :: rest ->
        Printf.printf "(%i, %i); " a b;
        print_tuples rest
  in
  print_tuples (product (range 0 3) (range 0 3))

let int_sqrt n = Float.to_int (Float.sqrt (Float.of_int n))
let () = print_int (int_sqrt 9)

let simple_sudoku =
  [|
    [| 9; 9; 7; 9; 8; 2; 4; 9; 9 |];
    [| 5; 4; 9; 3; 9; 1; 9; 9; 9 |];
    [| 9; 1; 0; 9; 7; 9; 2; 9; 9 |];
    [| 2; 7; 9; 9; 5; 9; 1; 9; 8 |];
    [| 9; 6; 9; 9; 9; 9; 9; 0; 9 |];
    [| 0; 9; 8; 9; 3; 9; 9; 6; 2 |];
    [| 9; 9; 4; 9; 0; 9; 6; 2; 9 |];
    [| 9; 9; 9; 2; 9; 8; 9; 1; 5 |];
    [| 9; 9; 5; 7; 1; 9; 0; 9; 9 |];
  |]

let () =
  assert (dpll_pure_unit (sudoku simple_sudoku));
  print_endline "simple_sudoku"
(*let () = print_endline (cnf_to_string (sudoku simple_sudoku))*)

let medium_sudoku =
  [|
    [| 9; 1; 9; 7; 4; 3; 9; 9; 9 |];
    [| 9; 9; 9; 5; 9; 9; 9; 9; 7 |];
    [| 9; 0; 9; 9; 9; 9; 9; 9; 8 |];
    [| 1; 9; 9; 9; 9; 9; 9; 8; 2 |];
    [| 9; 6; 4; 2; 9; 7; 5; 1; 9 |];
    [| 7; 8; 9; 9; 9; 9; 9; 9; 6 |];
    [| 3; 9; 9; 9; 9; 9; 9; 5; 9 |];
    [| 2; 9; 9; 9; 9; 1; 9; 9; 9 |];
    [| 9; 9; 9; 6; 5; 0; 9; 3; 9 |];
  |]

let () =
  assert (dpll_pure_unit (sudoku medium_sudoku));
  print_endline "medium_sudoku"

let hard_sudoku =
  [|
    [| 9; 9; 9; 9; 4; 9; 8; 9; 9 |];
    [| 8; 7; 9; 9; 9; 9; 9; 4; 9 |];
    [| 9; 3; 4; 6; 9; 9; 2; 9; 9 |];
    [| 9; 9; 3; 1; 9; 9; 7; 6; 9 |];
    [| 9; 9; 9; 0; 9; 3; 9; 9; 9 |];
    [| 9; 6; 5; 9; 9; 8; 0; 9; 9 |];
    [| 9; 9; 7; 9; 9; 0; 5; 2; 9 |];
    [| 9; 1; 9; 9; 9; 9; 9; 7; 0 |];
    [| 9; 9; 6; 9; 2; 9; 9; 9; 9 |];
  |]

let () =
  assert (dpll_pure_unit (sudoku hard_sudoku));
  print_endline "hard_sudoku"

let unsolvable_sudoku =
  [|
    [| 1; 9; 9; 8; 9; 9; 9; 9; 9 |];
    [| 9; 9; 9; 9; 9; 9; 9; 5; 9 |];
    [| 9; 9; 9; 9; 9; 0; 9; 9; 9 |];
    [| 4; 9; 1; 5; 9; 9; 3; 9; 6 |];
    [| 9; 9; 9; 9; 9; 3; 0; 9; 9 |];
    [| 9; 9; 9; 9; 8; 7; 9; 1; 2 |];
    [| 9; 9; 9; 9; 9; 2; 9; 7; 9 |];
    [| 9; 9; 4; 9; 0; 9; 9; 9; 9 |];
    [| 9; 9; 6; 9; 9; 9; 9; 9; 9 |];
  |]

let () =
  assert (not (dpll_pure_unit (sudoku unsolvable_sudoku)));
  print_endline "unsolvable_sudoku"
