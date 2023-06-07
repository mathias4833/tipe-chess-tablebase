open Int64;;
open Utils;;

(* Genere l'ensemble des coups pour la dame *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let n = Bitboard.get_lsb board in
      let moves = logor (Rook.moves_from_board ally whole n) (Bishop.moves_from_board ally whole n) in 
      generate_moves_aux (Bitboard.pop_lsb board) (Move.add_moves_to_list Q n moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wqueen chessboard.bqueen) []
;;
