open Int64;;
open Utils;;

let generate_mask  i j =
	let pos = 1L in
	let mask = ref 0L in
	for k=1 to 7 do 
		let a = i+k in
		let b = i-k in 
		let c = j+k in
		let d = j-k in
		if 0<a && 7>a && 0<c && 7>c then mask:= logor !mask (shift_left pos (a*8+c));
		if 0<a && 7>a && 0<d && 7>d then mask:= logor !mask (shift_left pos (a*8+d));
		if 0<b && 7>b && 0<c && 7>c then mask:= logor !mask (shift_left pos (b*8+c));
		if 0<b && 7>b && 0<d && 7>d then mask:= logor !mask (shift_left pos (b*8+d));
	done;
	!mask
;;

(* Cree la table des masques (tableau de generate_mask i j) *)
let table_mask =
  let create_table_mask n =
    let (i, j) = Bitboard.coord_of_index n in
    generate_mask i j
  in Array.init 64 create_table_mask 
;;


let generate_blockers_from_nearest i j dhd dhg dbd dbg =
  let blockers = ref 0L in
  for n = dhg+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i+n) (j-n));
  done;
  for n = dhd+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i+n) (j+n))
  done;
  for n = dbd+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i-n) (j+n))
  done;
  for n = dbg+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i-n) (j-n))
  done;
  !blockers
;;

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)
let generate_blockers i j =
  let mask = table_mask.(Bitboard.index_of_coord i j) in
  let blockers_list = ref [] in


  let borderline_down = if i=0 then 1 else 0 in
  let borderline_up = if i=7 then 1 else 0 in
  let bordercolumn_left = if j=0 then 1 else 0 in
  let bordercolumn_right = if j=7 then 1 else 0 in
  
  for dhg = 1 to (Int.min (7-i+borderline_up) (j+bordercolumn_left)) do 
    for dhd = 1 to (Int.min (7-i+borderline_up) (7-j+bordercolumn_right)) do
      for dbg = 1 to (Int.min (i+borderline_down) (j+bordercolumn_left)) do
        for dbd = 1 to (Int.min (i+borderline_down) (7-j+bordercolumn_right)) do
        
          let nearest_blockers = List.fold_left logor 0L [
            Bitboard.from_coordinate (i+dhd) (j+dhd);
            Bitboard.from_coordinate (i+dhg) (j-dhg);
            Bitboard.from_coordinate (i-dbd) (j+dbd);
            Bitboard.from_coordinate (i-dbg) (j-dbg)] in
          let full_blockers = logand mask (generate_blockers_from_nearest i j dhd dhg dbd dbg) in
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

  !blockers_list
;;


(* TODO: Commenter ! *)
(* Creation d'un tableau de dictionnaires contenant positions accessible *)
let table_moves =
  (* Cree le dictionnaire bloqueurs / cases accessibles *)
  let create_hashmap n =
    let i = n / 8 and j = n mod 8 in
    let all_blockers = generate_blockers i j in (* Ensemble des bloqueurs *)
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

(* Genere l'ensemble des positions accessibles pour un fou a l'indice n *)
let moves_from_board ally whole n =
    (* Bitboard contenant l'ensemble des pieces bloquantes *)
    let blockerboard = logand whole (table_mask.(n)) in
    logand (Hashtbl.find table_moves.(n) blockerboard) (lognot ally)
;;

(* Genere l'ensemble des coups pour le fou *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let n = Bitboard.get_lsb board in
      let moves = moves_from_board ally whole n in
      generate_moves_aux (Bitboard.pop_lsb board) (Move.add_moves_to_list B n moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wbishops chessboard.bbishops) []
;;
