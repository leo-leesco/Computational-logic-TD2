open Dpll

let cell_contains i j n =
  let i = i mod 9 in
  let j = j mod 9 in
  (9 * ((9 * i) + j)) + n

let product l1 l2 =
  List.concat (list_map (fun x -> list_map (fun y -> (x, y)) l2) l1)

let rec range min max =
  match max with n when n <= min -> [] | m -> (m - 1) :: range min (m - 1)

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
  let rows _ =
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
  let columns _ =
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
  let squares _ =
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
