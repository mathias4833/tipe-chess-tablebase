open Int64;;
open Utils;;

(* Table des coups possibles *)
let table_move =
  let create_table_move n =
    let (i, j) = Bitboard.coord_of_index n in
    Bitboard.from_coordinates [
      (i+1, j-2);
      (i+2, j-1);
      (i+2, j+1);
      (i+1, j+2);
      (i-1, j+2);
      (i-2, j+1);
      (i-2, j-1);
      (i-1, j-2);
    ]
  in Array.init 64 create_table_move
;;

(* Genere l'ensemble des coups pour le cavalier *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let n = Bitboard.get_lsb board in
      let all_moves = logand table_move.(n) (lognot ally) in
      generate_moves_aux (Bitboard.pop_lsb board) (Move.add_moves_to_list N n all_moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wknights chessboard.bknights) []
;;
