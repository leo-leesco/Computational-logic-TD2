type formula =
  | Var of int
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
  | Var _ -> raise Not_found
  | True -> true
  | False -> false
  | And (f1, f2) -> eval f1 && eval f2
  | Or (f1, f2) -> eval f1 || eval f2
  | Not f -> not (eval f)

let rec sat f =
  match free_var f with
  | Some v -> sat (subst v f True) || sat (subst v f False)
  | None -> eval f

