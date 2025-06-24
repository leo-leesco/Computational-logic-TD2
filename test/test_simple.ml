open TD2.Simple

let () =
  let x = Var 0 in
  let x' = Not x in
  let y = Var 1 in
  let y' = Not y in
  let a = And (And (Or (x, y), Or (x', y)), Or (x', y')) in
  let b = And (And (Or (x, y), Or (x', y)), And (Or (x, y'), Or (x', y'))) in
  assert (sat a);
  assert (not (sat b))
