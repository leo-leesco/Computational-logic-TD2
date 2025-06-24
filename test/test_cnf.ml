open TD2.Simple
open TD2.Cnf

let () =
  let x = Var 0 in
  let x' = Not x in
  let y = Var 1 in
  let y' = Not y in
  let f = And (And (Or (x, y), Or (x', y)), And (Or (x, y'), Or (x', y'))) in

  let cx = (true, 0) in
  let cx' = (false, 0) in
  let cy = (true, 1) in
  let cy' = (false, 1) in
  let cnf_f = [ [ cx; cy ]; [ cx'; cy ]; [ cx; cy' ]; [ cx'; cy' ] ] in
  assert (cnf_f = cnf f);
  let f' = Not (And (x, And (x', y))) in
  let cnf_f' = [ [ cx'; cx; cy' ] ] in
  assert (cnf_f' = cnf f')
