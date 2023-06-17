open Moves;;
open Utils;;

(* Genere les coups l'ensemble des coups legaux *)
let generate_legal_moves board =
  let rec remove_illegal_moves moves acc =
    match moves with
    |[] -> acc
    |h::t when Check.is_legal_move board h -> remove_illegal_moves t (h::acc)
    |_::t -> remove_illegal_moves t acc
  in
  remove_illegal_moves (King.generate_moves board)
    (remove_illegal_moves (Queen.generate_moves board)
    (remove_illegal_moves (Rook.generate_moves board)
    (remove_illegal_moves (Bishop.generate_moves board)
    (remove_illegal_moves (Knight.generate_moves board)
    (remove_illegal_moves (Pawn.generate_moves board) 
    (remove_illegal_moves (Castling.generate_moves board) []))))))
;;

Move.print_moves (Board.study_board) (generate_legal_moves Board.study_board);;

print_int (List.length (generate_legal_moves Board.study_board));;

