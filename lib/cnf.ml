open Simple
open Dpll
open Sudoku

let rec cnf f =
  match f with
  | Var v -> [ [ (true, v) ] ]
  | And (f1, f2) -> List.concat [ cnf f1; cnf f2 ]
  | Or (f1, f2) ->
      list_map
        (fun (c1, c2) -> List.concat [ c1; c2 ])
        (product (cnf f1) (cnf f2))
  | Not f -> (
      match f with
      | Var v -> [ [ (false, v) ] ]
      | Not g -> cnf g
      | And (f1, f2) -> cnf (Or (Not f1, Not f2))
      | Or (f1, f2) -> cnf (And (Not f1, Not f2))
      | True -> [ [] ]
      | False -> [])
  | True -> []
  | False -> [ [] ]
