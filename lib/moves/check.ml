open Int64;;
open Utils;;

(* Verfie si la position est legale. (Pas de roi mangeable) *)
let is_legal board =
  let enemy = Board.get_enemy_board board in
  let whole = Board.get_whole_board board in
  let n = Bitboard.get_lsb (Board.if_w_else board board.bking board.wking) in

  (* Aucune tour ni dame n'attaque le roi sur les lignes et les colonnes *)
  logand (logor (Board.if_w_else board board.wqueen board.bqueen) (Board.if_w_else board board.wrooks board.brooks))
      (Rook.moves_from_board enemy whole n) = 0L &&
  (* Aucun fou ni dame n'attaque le roi en diagonale *)
  logand (logor (Board.if_w_else board board.wqueen board.bqueen) (Board.if_w_else board board.wbishops board.bbishops))
      (Bishop.moves_from_board enemy whole n) = 0L &&
  (* Aucun cavalier n'attaque le roi *)
  logand (Board.if_w_else board board.wknights board.bknights) Knight.table_move.(n) = 0L &&
  (* Le roi adverse n'attaque pas le roi *)
  logand (Board.if_w_else board board.wking board.bking) King.table_move.(n) = 0L &&
  (* Aucun pion n'attaque le roi *) 
  logand (Board.if_w_else board board.wpawns board.bpawns) (Board.if_w_else board Pawn.table_btake.(n) Pawn.table_wtake.(n)) = 0L
;;

(* Verifie si le coup est legal *)
let is_legal_move board move =
  is_legal (Move.play_move board move)
;;

(* Verifie si le roi est un echecs *)
let is_check (board: Board.chessboard) =
  (* On regarde si c'etait a l'adversaire de jouer s'il pourrait manger le roi*)
  not (is_legal ({board with iswhite = not board.iswhite}))
;;
