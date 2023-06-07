open Int64;;
open Utils;;

(* Table des coups possibles *)
let table_move =
  let create_table_move n =
    let (i, j) = Bitboard.coord_of_index n in
    Bitboard.from_coordinates [
      (i  , j-1);
      (i+1, j-1);
      (i+1, j  );
      (i+1, j+1);
      (i  , j+1);
      (i-1, j+1);
      (i-1, j  );
      (i-1, j-1)
    ]
  in Array.init 64 create_table_move
;;

(* Genere l'ensemble des coups pour le roi *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  (* Indice du roi sur l'echiquier *)
  let n = Bitboard.get_lsb (Board.if_w_else chessboard chessboard.wking chessboard.bking) in
  Move.add_moves_to_list K n (logand table_move.(n) (lognot ally)) []  
;;
