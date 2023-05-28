open Int64;;
open Utils;;

(* Renvoie la position des cases accessible par prise *)
let capture_pawn iswhite (i, j) enemy acc =
  let bitboard =
    if iswhite then
      Bitboard.from_coordinates [(i+1, j-1); (i+1, j+1)]
    else
      Bitboard.from_coordinates [(i-1, j-1); (i-1, j+1)]
  in Bitboard.add_moves_to_list (logand bitboard enemy) acc
;;

(* Renvoie la position des cases accessible en avancant *)
let move_pawn iswhite (i, j) blockers acc =
  let bitboard =
    match (iswhite, i) with
    |(true, 1) -> Bitboard.from_coordinates [(i+1, j); (i+2, j)]
    |(true, _) -> Bitboard.from_coordinate (i+1) j
    |(_, 6) -> Bitboard.from_coordinates [(i-1, j); (i-2, j)]
    |_ -> Bitboard.from_coordinate (i-1) j
  in Bitboard.add_moves_to_list (logand bitboard (lognot blockers)) acc
;;

(* Genere l'ensemble des coups pour les pions *)
let generate_moves chessboard =
  let enemy = Board.get_enemy_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let (i, j) = Bitboard.coord_of_index (Bitboard.get_lsb board) in
    
      generate_moves_aux (Bitboard.pop_lsb board) (
        (* Genere la liste des coups pour le pion en (i, j) *)
        move_pawn chessboard.iswhite (i, j) whole ( 
          (capture_pawn chessboard.iswhite (i, j) enemy acc)
        )
      )
    )
  in generate_moves_aux (if chessboard.iswhite then
                           chessboard.wpawns
                         else chessboard.bpawns) []
;;

