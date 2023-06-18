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
    Bitboard.from_coordinate (i+1) j
  in Array.init 64 create_table_move
;;
let table_bmove =
  let create_table_move n =
    let (i, j) = Bitboard.coord_of_index n in
    Bitboard.from_coordinate (i-1) j
  in Array.init 64 create_table_move
;;


(* Renvoie le bitboard des coups possibles en avancant *)
let get_move_board iswhite whole n =
  let moveboard = 
    match iswhite with
    |true -> (
      let defaultmove = table_wmove.(n) in
      (* Verifie si le pion blanc peut avancer de deux cases *)
      if 7 < n && n < 16 && (logand defaultmove whole = 0L) then
        logor defaultmove table_wmove.(n+8)
      else
        defaultmove
    )
    |_ -> (
      let defaultmove = table_bmove.(n) in
      (* Verifie si le pion noir peut avancer de deux cases *)
      if 47 < n && n < 56 && (logand defaultmove whole = 0L) then
        logor defaultmove table_bmove.(n-8)
      else
        defaultmove
    )
  in logand moveboard (lognot whole)
;;

(* Genere l'ensemble des coups pour les pions *)
let generate_moves chessboard =
  let enemy = Board.get_enemy_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      (* Indice du pion *)
      let n = Bitboard.get_lsb board in
      (* Bitboards des coups par prise / sans prise en fonction de la couleur *)
      let takeboard = Board.if_w_else chessboard table_wtake.(n) table_btake.(n) in
      let moveboard = get_move_board chessboard.iswhite whole n in
      
      (* Bitboard resultant des coups possibles *)
      let all_moves = logor (logand takeboard enemy) moveboard in
     
      (* Verifie si il y a promotion *)
      if Board.if_w_else chessboard (n > 47) (n < 16) then
        generate_moves_aux (Bitboard.pop_lsb board)
          (Move.add_moves_to_list Q n all_moves
          (Move.add_moves_to_list R n all_moves   
          (Move.add_moves_to_list B n all_moves
          (Move.add_moves_to_list N n all_moves acc))))
      else
        generate_moves_aux (Bitboard.pop_lsb board) (Move.add_moves_to_list P n all_moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wpawns chessboard.bpawns) []
;;
