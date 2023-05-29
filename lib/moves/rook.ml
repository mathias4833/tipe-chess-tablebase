open Int64;;
open Utils;;

(* Genere le masque associe aux coordonnees i j, cases au bord inclues *)
let generate_complete_mask i j =
  let n = 8 * i + j in
  (* Remplace la ligne et la colonne ou se trouve la piece par des 1 *)
  let mask = logor (shift_left Bitboard.line (8*i)) (shift_left Bitboard.column j) in

  (* Supprime la case ou se trouve la piece *)
  logand mask (lognot (shift_left 1L n)) 
;;

(* Genere le masque associe aux coordonnees i j, cases au bord exclues *)
let generate_partial_mask i j =
  let mask = ref (generate_complete_mask i j) in

  (* Supprime les bords si la case n'est pas sur un bord,
    reduit le nombre de positions possibles *)
  if i != 0 then
    mask := logand !mask (lognot Bitboard.line);
  if i != 7 then
    mask := logand !mask (lognot (shift_left Bitboard.line (8*7)));
  if j != 0 then
    mask := logand !mask (lognot Bitboard.column);
  if j != 7 then
    mask := logand !mask (lognot (shift_left Bitboard.column 7));
  !mask
;;

(* Cree la table des masques (tableau de generate_mask i j) *)
let table_mask =
  let create_table_mask n =
    let (i, j) = Bitboard.coord_of_index n in
    generate_partial_mask i j
  in Array.init 64 create_table_mask 
;;

(* Genere le bitboard contenant tous les bloqueurs sauf les plus pres de la piece *)
let generate_blockers_from_nearest i j l1 l2 c1 c2 =
  let blockers = ref 0L in
  for n = 1 to l1 - 1 do
    blockers := logor !blockers (Bitboard.from_coordinate n j);
  done;
  for n = l2 + 1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate n j)
  done;
  for n = 1 to c1 - 1 do
    blockers := logor !blockers (Bitboard.from_coordinate i n)
  done;
  for n = c2 + 1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate i n)
  done;
  !blockers
;;

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)
let generate_blockers i j =
  let mask = table_mask.(Bitboard.index_of_coord i j) in
  let blockers_list = ref [] in

  let start_line = if i = 0 then -1 else 0 in
  let stop_line = if i = 7 then 8 else 7 in
  let start_column = if j = 0 then -1 else 0 in
  let stop_column = if j = 7 then 8 else 7 in
  
  for l1 = start_line to (i-1) do
    for l2 = stop_line downto (i+1) do
      for c1 = start_column to (j-1) do
        for c2 = stop_column downto (j+1) do
          let nearest_blockers = List.fold_left logor 0L [
            Bitboard.from_coordinate l1 j;
            Bitboard.from_coordinate l2 j;
            Bitboard.from_coordinate i c1;
            Bitboard.from_coordinate i c2] in
          
          let full_blockers = generate_blockers_from_nearest i j l1 l2 c1 c2 in 
          let accessible_mask = logand (logor mask nearest_blockers) (lognot full_blockers) in
   
          (* Liste contenant les bloqueurs possibles *)
          let blockers =
            let rec aux l acc =
              match l with
              |[] -> acc
              |h::t -> aux t ((logand mask (logor h nearest_blockers))::acc)
            in aux (Bitboard.generate_combinations full_blockers) []
          in
          
          blockers_list := (accessible_mask, blockers)::!blockers_list
        done;
      done;
    done;
  done;

  (mask, !blockers_list)
;;


(* TODO: Commenter ! *)
(* Creation d'un tableau de dictionnaires contenant positions accessible *)
let table_moves =
  (* Cree le dictionnaire bloqueurs / cases accessibles *)
  let create_hashmap n =
    let i = n / 8 and j = n mod 8 in
    let (_, all_blockers) = generate_blockers i j in (* Ensemble des bloqueurs *)
    let hashmap = Hashtbl.create 1024 in (* Dictionnaire vide *)

    let rec aux1 l accessible hashmap =
      match l with
      |[] -> hashmap
      |h::t -> (
        Hashtbl.add hashmap h accessible;
        aux1 t accessible hashmap
      )
    and aux2 l hashmap =
      match l with
      |[] -> hashmap
      |(accessible, blockers)::t -> aux2 t (aux1 blockers  accessible hashmap)
    in aux2 all_blockers hashmap
  in Array.init 64 create_hashmap 
;;


(* Genere l'ensemble des coups pour la tour *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let n = Bitboard.get_lsb board in
      (* Bitboard contenant l'ensemble des pieces bloquantes *)
      let blockerboard = logand whole (table_mask.(n)) in
      let all_moves = logand (Hashtbl.find table_moves.(n) blockerboard) (lognot ally) in

      generate_moves_aux (Bitboard.pop_lsb board) (Bitboard.add_moves_to_list all_moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wrooks chessboard.brooks) []
;;
