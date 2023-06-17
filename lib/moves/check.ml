open Int64;;
open Utils;;

(* Verfie si la position est legale. (Pas de roi en echecs) *)
let is_legal_move board move =
  let b = Move.play_move board move in
  let enemy = Board.get_enemy_board b in
  let whole = Board.get_whole_board b in
  let n = Bitboard.get_lsb (Board.if_w_else b b.bking b.wking) in
  
  (* Aucune tour ni dame n'attaque le roi sur les lignes et les colonnes *)
  logand (logor (Board.if_w_else b b.wqueen b.bqueen) (Board.if_w_else b b.wrooks b.brooks))
      (Rook.moves_from_board enemy whole n) = 0L &&
  (* Aucun fou ni dame n'attaque le roi en diagonale *)
  logand (logor (Board.if_w_else b b.wqueen b.bqueen) (Board.if_w_else b b.wbishops b.bbishops))
      (Bishop.moves_from_board enemy whole n) = 0L &&
  (* Aucun cavalier n'attaque le roi *)
  logand (Board.if_w_else b b.wknights b.bknights) Knight.table_move.(n) = 0L &&
  (* Le roi adverse n'attaque pas le roi *)
  logand (Board.if_w_else b b.wking b.bking) King.table_move.(n) = 0L &&
  (* Aucun pion n'attaque le roi *) 
  logand (Board.if_w_else b b.wpawns b.bpawns) (Board.if_w_else b Pawn.table_btake.(n) Pawn.table_wtake.(n)) = 0L
;;