type var = int

type formula =
  | Var of var
  | And of (formula * formula)
  | Or of (formula * formula)
  | Not of formula
  | True
  | False

let rec subst x a b =
  match a with
  | Var v when x = v -> b
  | Not f -> Not (subst x f b)
  | And (f1, f2) -> And (subst x f1 b, subst x f2 b)
  | Or (f1, f2) -> Or (subst x f1 b, subst x f2 b)
  | b -> b

let rec free_var f =
  match f with
  | Var v -> Some v
  | True | False -> None
  | And (f1, f2) -> ( match free_var f1 with None -> free_var f2 | v -> v)
  | Or (f1, f2) -> ( match free_var f1 with None -> free_var f2 | v -> v)
  | Not f -> free_var f

let rec eval f =
  match f with
  | Var v -> raise Not_found
  | True -> true
  | False -> false
  | And (f1, f2) -> eval f1 && eval f2
  | Or (f1, f2) -> eval f1 || eval f2
  | Not f -> not (eval f)

let rec sat f =
  match free_var f with
  | Some v -> sat (subst v f True) || sat (subst v f False)
  | None -> eval f

let () =
  let x = Var 0 in
  let x' = Not x in
  let y = Var 1 in
  let y' = Not y in
  let a = And (And (Or (x, y), Or (x', y)), Or (x', y')) in
  let b = And (And (Or (x, y), Or (x', y)), And (Or (x, y'), Or (x', y'))) in
  assert (sat a);
  assert (not (sat b))

type literal = bool * var (* false means negated *)
type clause = literal list
type cnf = clause list

let rec list_mem x lst =
  match lst with [] -> false | head :: tail -> x = head || list_mem x tail

let () =
  assert (list_mem 2 [ 1; 2; 3 ]);
  assert (not (list_mem 5 [ 1; 2; 3 ]));
  assert (not (list_mem 1 []))

let rec list_map f lst =
  match lst with head :: tail -> f head :: list_map f tail | [] -> []

let () =
  assert (list_map (fun x -> 2 * x) [ 1; 2; 3 ] = [ 2; 4; 6 ]);
  assert (list_map (fun _ -> ()) [] = [])

let rec list_filter filtre lst =
  match lst with
  | head :: tail ->
      if filtre head then head :: list_filter filtre tail
      else list_filter filtre tail
  | [] -> []

let () =
  let even x = x mod 2 = 0 in
  assert (list_filter even [ 1; 2; 3; 4; 6 ] = [ 2; 4; 6 ])

let subst_cnf x b normal_form =
  list_map
    (list_filter (fun v -> not (v = (not b, x))))
    (list_filter (fun c -> not (list_mem (b, x) c)) normal_form)

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in
  assert (subst_cnf 0 true a = [ [ y ]; [ y' ] ]);
  assert (subst_cnf 0 false a = [ [ y ] ])

let rec dpll normal_form =
  match normal_form with
  | [] -> true
  | c1 :: remainder -> (
      match c1 with
      | [] -> false
      | (_, head) :: tail ->
          dpll (subst_cnf head true normal_form)
          || dpll (subst_cnf head false normal_form))

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in
  let b = [ [ x; y ]; [ x'; y ]; [ x; y' ]; [ x'; y' ] ] in
  assert (dpll a);
  assert (not (dpll b))

let rec unit normal_form =
  match normal_form with
  | [] -> raise Not_found
  | c :: tail -> if List.length c = 1 then List.hd c else unit tail

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

let rec pure normal_form =
  match normal_form with
  | [] -> raise Not_found
  | c :: tail -> (
      match c with
      | [] -> pure tail
      | (b, v) :: rest ->
          if
            (not (list_mem (not b, v) rest))
            && not (List.exists (list_mem (not b, v)) tail)
          then (b, v)
          else
            pure
              (list_map
                 (fun c -> list_filter (fun (b', v') -> v' <> v) c)
                 normal_form))

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

let rec dpll_pure_unit normal_form =
  if list_mem [] normal_form then false
  else
    try
      let n, v = unit normal_form in
      dpll_pure_unit (subst_cnf v n normal_form)
    with Not_found -> (
      try
        let n, v = pure normal_form in
        dpll_pure_unit (subst_cnf v n normal_form)
      with Not_found -> (
        match normal_form with
        | [] -> true
        | c1 :: remainder -> (
            match c1 with
            | [] -> false
            | (_, head) :: tail ->
                dpll_pure_unit (subst_cnf head true normal_form)
                || dpll_pure_unit (subst_cnf head false normal_form))))

let () =
  let x = (true, 0) in
  let x' = (false, 0) in
  let y = (true, 1) in
  let y' = (false, 1) in
  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in
  let b = [ [ x; y ]; [ x'; y ]; [ x; y' ]; [ x'; y' ] ] in
  assert (dpll_pure_unit a);
  assert (not (dpll_pure_unit b))

let cnf_to_string norm_form =
  "("
  ^ String.concat ")\n\u{2227}("
      (list_map (String.concat "\u{2228}")
         (list_map
            (fun c ->
              list_map
                (fun (b, v) ->
                  (if b then "" else "\u{00ac}") ^ "x" ^ string_of_int v)
                c)
            norm_form))
  ^ ")"

(*let () =*)
(*  let x = (true, 0) in*)
(*  let x' = (false, 0) in*)
(*  let y = (true, 1) in*)
(*  let y' = (false, 1) in*)
(*  let a = [ [ x; y ]; [ x'; y ]; [ x'; y' ] ] in*)
(*  print_endline (cnf_to_string a)*)

(** Parse a CNF file. *)
let parse f : cnf =
  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
  in
  let f = load_file f in
  let f = String.map (function '\t' -> ' ' | c -> c) f in
  let f = String.split_on_char '\n' f in
  let f = List.map (String.split_on_char ' ') f in
  let f = List.filter (function "c" :: _ | "p" :: _ -> false | _ -> true) f in
  let f = List.flatten f in
  let aux (a, c) = function
    | "" -> (a, c)
    | "0" -> (c :: a, [])
    | n ->
        let n = int_of_string n in
        let x = if n < 0 then (false, -n) else (true, n) in
        (a, x :: c)
  in
  fst (List.fold_left aux ([], []) f)

let () =
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

let cell_contains i j n =
  let i = i mod 9 in
  let j = j mod 9 in
  (9 * ((9 * i) + j)) + n

let product l1 l2 =
  List.concat (list_map (fun x -> list_map (fun y -> (x, y)) l2) l1)

let rec range min max =
  match max with n when n <= min -> [] | m -> (m - 1) :: range min (m - 1)

(*let () =*)
(*  let rec print_tuples = function*)
(*    | [] -> ()*)
(*    | (a, b) :: rest ->*)
(*        Printf.printf "(%i, %i); " a b;*)
(*        print_tuples rest*)
(*  in*)
(*  print_tuples (product (range 0 3) (range 0 3))*)

(*Pour une raison qui m'échappe, même le sudoku simple n'a pas une CNF associée satisfiable

  Voici le processus de formation des clauses :
  - on crée un ensemble d'indices (product (range 0 n) (range 0 n) par exemple) sur lequel on itère
  - on applique la condition souhaitée, éventuellement avec une exclusion d'indices qui résulte en une liste vide
  - on filtre les clauses vides

  les règles implémentées sont :
  - on impose que chaque case vale soit la valeur de grid, soit n'importe quelle valeur (remplissage + clues)
  - on impose que dans chaque ligne, on ait une unique fois chaque valeur (à i fixé, si j1!=j2, x(i,j1)=k -> ¬x(i,j2)=k)
  - on impose que dans chaque colonne, on ait une unique fois chaque valeur (à j fixé, si i1!=i2, x(i1,j)=k -> ¬x(i2,j)=k)
  - on impose que dans chaque carré, on ait une unique fois chaque valeur (à (a,b) fixé, si (i1,j1)!=(i2,j2), x(3*a+i1,3*b+j1)=k -> ¬x(3*a+i2,3*b+j2)=k)
  On remplace a->¬b par ¬a∨¬b
*)
let sudoku grid =
  let n = Array.length grid in
  let clues constraints =
    list_map
      (fun (i, j) ->
        if constraints.(i).(j) < 9 then
          [ (true, cell_contains i j constraints.(i).(j)) ]
        else list_map (fun k -> (true, cell_contains i j k)) (range 0 n))
      (product (range 0 n) (range 0 n))
  in
  let rows size =
    list_filter
      (fun l -> not (List.is_empty l))
      (list_map
         (fun ((j1, j2), (i, k)) ->
           if j1 < j2 then
             [ (false, cell_contains i j1 k); (false, cell_contains i j2 k) ]
           else [])
         (product
            (product (range 0 n) (range 0 n))
            (product (range 0 n) (range 0 n))))
  in
  let columns size =
    list_filter
      (fun l -> not (List.is_empty l))
      (list_map
         (fun ((i1, i2), (j, k)) ->
           if i1 < i2 then
             [ (false, cell_contains i1 j k); (false, cell_contains i2 j k) ]
           else [])
         (product
            (product (range 0 n) (range 0 n))
            (product (range 0 n) (range 0 n))))
  in
  let squares size =
    let int_sqrt i = Float.to_int (Float.sqrt (Float.of_int i)) in
    list_filter
      (fun l -> not (List.is_empty l))
      (list_map
         (fun (((a, b), ((i1, j1), (i2, j2))), k) ->
           if (i1, j1) <> (i2, j2) then
             [
               ( false,
                 cell_contains ((a * int_sqrt n) + i1) ((b * int_sqrt n) + j1) k
               );
               ( false,
                 cell_contains ((a * int_sqrt n) + i2) ((b * int_sqrt n) + j2) k
               );
             ]
           else [])
         (product
            (product
               (product (range 0 (int_sqrt n)) (range 0 (int_sqrt n)))
               (product
                  (product (range 0 (int_sqrt n)) (range 0 (int_sqrt n)))
                  (product (range 0 (int_sqrt n)) (range 0 (int_sqrt n)))))
            (range 0 n)))
  in
  List.concat [ rows n; columns n; squares n; clues grid ]

(*let int_sqrt n = Float.to_int (Float.sqrt (Float.of_int n))*)
(*let () = print_int (int_sqrt 9)*)

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
