type literal = bool * int (* false means negated *)
type clause = literal list
type cnf = clause list

let rec list_mem x lst =
  match lst with [] -> false | head :: tail -> x = head || list_mem x tail

let rec list_map f lst =
  match lst with head :: tail -> f head :: list_map f tail | [] -> []

let rec list_filter filtre lst =
  match lst with
  | head :: tail ->
      if filtre head then head :: list_filter filtre tail
      else list_filter filtre tail
  | [] -> []

let subst_cnf x b normal_form =
  list_map
    (list_filter (fun v -> not (v = (not b, x))))
    (list_filter (fun c -> not (list_mem (b, x) c)) normal_form)

let rec dpll normal_form =
  match normal_form with
  | [] -> true
  | c1 :: _ -> (
      match c1 with
      | [] -> false
      | (_, head) :: _ ->
          dpll (subst_cnf head true normal_form)
          || dpll (subst_cnf head false normal_form))

let rec unit normal_form =
  match normal_form with
  | [] -> raise Not_found
  | c :: tail -> if List.length c = 1 then List.hd c else unit tail

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
                 (fun c -> list_filter (fun (_, v') -> v' <> v) c)
                 normal_form))

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
        | c1 :: _ -> (
            match c1 with
            | [] -> false
            | (_, head) :: _ ->
                dpll_pure_unit (subst_cnf head true normal_form)
                || dpll_pure_unit (subst_cnf head false normal_form))))

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

  let f =
    load_file f
    |> String.map (function '\t' -> ' ' | c -> c)
    |> String.split_on_char '\n'
    |> List.map (String.split_on_char ' ')
    |> List.filter (function "c" :: _ | "p" :: _ -> false | _ -> true)
    |> List.flatten
  in

  let aux (a, c) = function
    | "" -> (a, c)
    | "0" -> (c :: a, [])
    | n ->
        let n = int_of_string n in
        let x = if n < 0 then (false, -n) else (true, n) in
        (a, x :: c)
  in
  fst (List.fold_left aux ([], []) f)
