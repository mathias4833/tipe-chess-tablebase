open Utils;;

(* Genere les coups de roque si possible *)
let generate_moves (chessboard: Board.chessboard) =
  if chessboard.iswhite then
    (* Le roi n'a pas encore bouge *)
    if chessboard.wcastle then (
      let whole = Board.get_whole_board chessboard in
      (* Verifie que la tour est a sa place et qu'aucune piece ne bloque et que le roi ne se met pas en echecs *)
      let castling_moves =
        (* Petit roque *)
        if (Bitboard.get_nth chessboard.wrooks 7) = 1L
          && (Bitboard.get_nth whole 5) = 0L
          && (Bitboard.get_nth whole 6) = 0L
          && not (Check.is_check_move chessboard (Move.Chessmove(K, 4, 5)))
          && not (Check.is_check_move chessboard (Move.Chessmove(K, 4, 6)))
        then [Move.ShortCastling] else [] in
      (* Grand roque *)
      if (Bitboard.get_nth chessboard.wrooks 0) = 1L
        && (Bitboard.get_nth whole 1) = 0L
        && (Bitboard.get_nth whole 2) = 0L
        && (Bitboard.get_nth whole 3) = 0L
        && not (Check.is_check_move chessboard (Move.Chessmove(K, 4, 1)))
        && not (Check.is_check_move chessboard (Move.Chessmove(K, 4, 2)))
        && not (Check.is_check_move chessboard (Move.Chessmove(K, 4, 3)))
      then
        (Move.LongCastling::castling_moves)
      else
        castling_moves
    ) else []
  else (
    (* Le roi n'a pas encore bouge *)
    if chessboard.bcastle then (
      let whole = Board.get_whole_board chessboard in
      (* Verifie que la tour est a sa place et qu'aucune piece ne bloque et que le roi ne se met pas en echecs *)
      let castling_moves =
        (* Petit roque *)
        if (Bitboard.get_nth chessboard.brooks 63) = 1L
          && (Bitboard.get_nth whole 61) = 0L
          && (Bitboard.get_nth whole 62) = 0L
          && not (Check.is_check_move chessboard (Move.Chessmove(K, 60, 61)))
          && not (Check.is_check_move chessboard (Move.Chessmove(K, 60, 61)))
        then [Move.ShortCastling] else [] in
      (* Grand roque *)
      if (Bitboard.get_nth chessboard.wrooks 56) = 1L
        && (Bitboard.get_nth whole 57) = 0L
        && (Bitboard.get_nth whole 58) = 0L
        && (Bitboard.get_nth whole 59) = 0L
        && not (Check.is_check_move chessboard (Move.Chessmove(K, 60, 57)))
        && not (Check.is_check_move chessboard (Move.Chessmove(K, 60, 58)))
        && not (Check.is_check_move chessboard (Move.Chessmove(K, 60, 59)))
      then
        (Move.LongCastling::castling_moves)
      else
        castling_moves
    ) else []
  )
;;
