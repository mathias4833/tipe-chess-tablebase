open Int64;;


(* Colonne et ligne de 1s *)
let column = 0x101010101010101L;;
let line = 0xffL;;

(* Transforme une coordonnee en un indice *)
let index_of_coord i j =
  i * 8 + j
;;

(* Transforme un indice en coordonnee *)
let coord_of_index n =
  (n / 8, n mod 8)
;;

(* Renvoie la valeur du nth bit *)
let get_nth x n =
    logand (shift_right x n) 1L
;;

(* Change la valeur du nth bit par un 1 *)
let set_nth x n =
  logor x (shift_left 1L n)
;;

(* Change la valeur du nth bit par 0 *)
let clear_nth x n =
    logand x (lognot (shift_left 1L n))
;;

(* Renvoie le bitboard en echangeant la valeur du bit de poids faible *)
let pop_lsb x =
   logand x (sub x 1L)
;;

(* Renvoie l'indice du bit de poids faible, commence a 0 *)
let get_lsb x =
  let rec get_lsb_aux x acc =
    match logand x 1L with
    |1L -> acc
    |_ -> get_lsb_aux (shift_right x 1) (acc+1)
  in match x with
  |0L -> 0
  |_ -> get_lsb_aux x 0
;;

(* Compte le nombre de 1 *)
let rec count_ones x =
    match x with
    |0L -> 0
    |x when (logand x 1L) = 1L -> 1 + count_ones (shift_right x 1)
    |_ -> count_ones (shift_right x 1)
;;

(* Cree un bitboard avec un 1 en position i j *)
let from_coordinate i j =
  if i < 0 || i > 7 || j < 0 || j > 7 then
    0L
  else
    shift_left 1L (index_of_coord i j)
;;

(* Cree un bitboard a partir d'une liste de coordonnees *)
let from_coordinates l =
  let rec from_coordinates_aux l acc =
    match l with
    |[] -> acc
    |(i, j)::t when i < 0 || i > 7 || j < 0 || j > 7 -> from_coordinates_aux t acc 
    |(i, j)::t -> from_coordinates_aux t (set_nth acc (index_of_coord i j))
  in from_coordinates_aux l 0L
;;

(* Print le bitboard *)
let print_board b =
  (* Print la ligne i du bitboard *)
  let rec line i b =
    match i, b with
    |(i, _) when i < 0 -> "\n"
    |(_, b) when (logand b 1L) = 1L -> "1" ^ (line (i-1) (shift_right b 1))
    |_ -> "." ^ (line (i-1) (shift_right b 1))
  (* Print le bitboard ligne par ligne *)
  and print_bitboard_aux i b =
    match (i, b) with
    |(i, _) when i < 0 -> ""
    |_ -> (print_bitboard_aux (i-1) (shift_right b 8)) ^ (line 7 b)
  in print_string ("--------\n" ^ (print_bitboard_aux 7 b) ^ "--------\n") 
;;

(* Print une liste de bitboard *)
let rec print_list_board l =
  match l with
  |[] -> ()
  |h::t -> print_board h; print_string "\n" ; print_list_board t
;;


(* Genere la liste des combinaisons de 0 et de 1 à partir d'un nombre donne *)
let generate_combinations bitboard =
  (* Nombre de combinaisons possible, n = 2^p avec p le nombre de 1 *)
  let n = to_int (shift_left 1L (count_ones bitboard)) in
  (* Cree la combinaison associee au nombre, 0 <= x < n pour avoir toutes les combinaisons *)
  let rec generate_combinations_aux bitboard x acc =
    let index = ref 0 in
    let combination = ref bitboard in
    (* Parcours du nombre pour remplacer les 1 par des 0 *)
    for k = 0 to 63 do
      (* Si le k-ieme bit de !combination est un 1 alors que celui de x est un 0 *)
      if (get_nth !combination k) = 1L then (
        if (get_nth (of_int x) !index) = 0L then (
          (* On remplace le k-ieme bit par un 0 *)
          combination := clear_nth !combination k;
        );
        index := !index + 1
      );
    done;
    match x with
    |0 -> ((!combination)::acc)
    |_ -> generate_combinations_aux bitboard (x-1) ((!combination)::acc)
  in generate_combinations_aux bitboard (n-1) [] 
;;

