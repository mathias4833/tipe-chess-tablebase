open Int64;;
open Utils;;

(* Table des coups possible par prise *)
let table_wtake =
  let create_table_capture n =
    let (i, j) = Bitboard.coord_of_index n in
    Bitboard.from_coordinates [(i+1, j-1); (i+1, j+1)]
  in Array.init 64 create_table_capture
;;
let table_btake =
  let create_table_capture n =
    let (i, j) = Bitboard.coord_of_index n in
    Bitboard.from_coordinates [(i-1, j-1); (i-1, j+1)]
  in Array.init 64 create_table_capture
;;

(* Table des coups possible sans prise *)
let table_wmove =
  let create_table_move n =
    let (i, j) = Bitboard.coord_of_index n in
    match i with
    |1 -> Bitboard.from_coordinates [(i+1, j); (i+2, j)]
    |_ -> Bitboard.from_coordinate (i+1) j
  in Array.init 64 create_table_move
;;
let table_bmove =
  let create_table_move n =
    let (i, j) = Bitboard.coord_of_index n in
    match i with
    |6 -> Bitboard.from_coordinates [(i-1, j); (i-2, j)]
    |_ -> Bitboard.from_coordinate (i-1) j
  in Array.init 64 create_table_move
;;


(* Genere l'ensemble des coups pour les pions *)
let generate_moves chessboard =
  let enemy = Board.get_enemy_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let n = Bitboard.get_lsb board in
      (* Bitboards des coups par prise / sans prise en fonction de la couleur *)
      let takeboard = Board.if_w_else chessboard table_wtake.(n) table_btake.(n) in
      let moveboard = Board.if_w_else chessboard table_wmove.(n) table_bmove.(n) in

      (* Bitboard resultant des coups possibles *)
      let all_moves = logor (logand takeboard enemy) (logand moveboard (lognot whole)) in
      generate_moves_aux (Bitboard.pop_lsb board) (Move.add_moves_to_list P n all_moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wpawns chessboard.bpawns) []
;;

