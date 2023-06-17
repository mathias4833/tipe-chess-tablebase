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

let (board: Board.chessboard) = {
    wpawns = 0L;
    wknights = 0L;
    wbishops = 0L;
    wrooks = 0x81L;
    wqueen = 0L;
    wking = 0x10L;
    bpawns = 0L;
    bknights = 0L;
    bbishops = 0L;
    brooks = 0x4000000000000000L;
    bqueen = 0L;
    bking = 0x1000000000000000L;
    iswhite = true;
    wcastle = true;
    bcastle = true
};;


Move.print_moves (board) (generate_legal_moves board);;

