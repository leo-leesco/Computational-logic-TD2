open TD2.Dpll

let () =
  assert (list_mem 2 [ 1; 2; 3 ]);
  assert (not (list_mem 5 [ 1; 2; 3 ]));
  assert (not (list_mem 1 []))

let () =
  assert (list_map (fun x -> 2 * x) [ 1; 2; 3 ] = [ 2; 4; 6 ]);
  assert (list_map (fun _ -> ()) [] = [])

let () =
  let even x = x mod 2 = 0 in
  assert (list_filter even [ 1; 2; 3; 4; 6 ] = [ 2; 4; 6 ])

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in
  assert (subst_cnf 0 true a = [ [ y ]; [ y' ] ]);
  assert (subst_cnf 0 false a = [ [ y ] ])

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in
  let b = [ [ x; y ]; [ x'; y ]; [ x; y' ]; [ x'; y' ] ] in
  assert (dpll a);
  assert (not (dpll b))

let () =
  let x = (true, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  assert (unit [ [ x; y ]; [ x ]; [ y; y' ] ] = x);
  assert (
    try
      let _ = unit [] in
      false
    with Not_found -> true);
  assert (
    try
      let _ = unit [ [] ] in
      false
    with Not_found -> true)

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let z = (true, 2) in
  assert (pure [ [ x; y ]; [ x ]; [ y; y' ] ] = x);
  assert (pure [ [ y; x ]; [ z ]; [ x' ]; [ y; y' ] ] = z);
  assert (
    try
      let _ = pure [ [ x; x' ] ] in
      false
    with Not_found -> true);
  assert (
    try
      let _ = pure [] in
      false
    with Not_found -> true);
  assert (
    try
      let _ = pure [ [] ] in
      false
    with Not_found -> true)

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in
  let b = [ [ x; y ]; [ x'; y ]; [ x; y' ]; [ x'; y' ] ] in
  assert (dpll_pure_unit a);
  assert (not (dpll_pure_unit b))

(*let () =*)
(*  let x = (true, 0) in*)
(*  let x' = (false, 0) in*)
(*  let y = (true, 1) in*)
(*  let y' = (false, 1) in*)
(*  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in*)
(*  print_endline (cnf_to_string a)*)

let () =
  (* print_endline (Sys.getcwd ()); *)
  assert (dpll_pure_unit (parse "sat/ais12.cnf"));
  print_endline "sat/ais12.cnf";
  assert (dpll_pure_unit (parse "sat/flat50-1000.cnf"));
  print_endline "sat/flat50-1000.cnf";
  assert (dpll_pure_unit (parse "sat/ii8a2.cnf"));
  print_endline "sat/ii8a2.cnf";
  assert (dpll_pure_unit (parse "sat/quinn.cnf"));
  print_endline "sat/quinn.cnf";
  assert (dpll_pure_unit (parse "sat/zebra_v155_c1135.cnf"));
  print_endline "sat/zebra_v155_c1135.cnf"

let () =
  assert (not (dpll_pure_unit (parse "unsat/aim-50-1_6-no-1.cnf")));
  print_endline "unsat/aim-50-1_6-no-1.cnf";
  (*assert (not (dpll_pure_unit (parse "unsat/bf1355-075.cnf")));*)
  (*print_endline "unsat/bf1355-075.cnf";*)
  assert (not (dpll_pure_unit (parse "unsat/dubois20.cnf")));
  print_endline "unsat/dubois20.cnf";
  assert (not (dpll_pure_unit (parse "unsat/dubois21.cnf")));
  print_endline "unsat/dubois21.cnf";
  assert (not (dpll_pure_unit (parse "unsat/hole6.cnf")));
  print_endline "unsat/hole6.cnf"
