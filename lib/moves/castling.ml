open Utils

(** [generate_moves chessboard] génère les coups de roques possibles
    @param chessboard Le plateau d'échecs.
    @return Liste des roques possibles (petit ou grand roque). *)
let generate_moves (chessboard : Board.chessboard) =
  if Board.is_white chessboard then
    (* Le roi n'a pas encore bouge *)
    if chessboard.wcastle then
      let whole = Board.get_whole_board chessboard in
      (* Verifie que la tour est a sa place et qu'aucune piece ne bloque et que le roi ne se met pas en echecs *)
      let castling_moves =
        (* Petit roque *)
        if
          Bitboard.get_nth chessboard.wrooks 7 = 1L
          && Bitboard.get_nth whole 5 = 0L
          && Bitboard.get_nth whole 6 = 0L
          && Check.is_legal_move chessboard (Chessmove (K, 4, 4, None))
          && Check.is_legal_move chessboard (Chessmove (K, 4, 5, None))
          && Check.is_legal_move chessboard (Chessmove (K, 4, 6, None))
        then [ Board.ShortCastling ]
        else []
      in
      (* Grand roque *)
      if
        Bitboard.get_nth chessboard.wrooks 0 = 1L
        && Bitboard.get_nth whole 1 = 0L
        && Bitboard.get_nth whole 2 = 0L
        && Bitboard.get_nth whole 3 = 0L
        && Check.is_legal_move chessboard (Chessmove (K, 4, 4, None))
        && Check.is_legal_move chessboard (Chessmove (K, 4, 3, None))
        && Check.is_legal_move chessboard (Chessmove (K, 4, 2, None))
      then Board.LongCastling :: castling_moves
      else castling_moves
    else [] (* Le roi a deja joue, pas de roque possible *)
  else if (* Le roi n'a pas encore bouge *)
          chessboard.bcastle then
    let whole = Board.get_whole_board chessboard in
    (* Verifie que la tour est a sa place et qu'aucune piece ne bloque et que le roi ne se met pas en echecs *)
    let castling_moves =
      (* Petit roque *)
      if
        Bitboard.get_nth chessboard.brooks 63 = 1L
        && Bitboard.get_nth whole 61 = 0L
        && Bitboard.get_nth whole 62 = 0L
        && Check.is_legal_move chessboard (Chessmove (K, 60, 60, None))
        && Check.is_legal_move chessboard (Chessmove (K, 60, 61, None))
        && Check.is_legal_move chessboard (Chessmove (K, 60, 62, None))
      then [ Board.ShortCastling ]
      else []
    in
    (* Grand roque *)
    if
      Bitboard.get_nth chessboard.wrooks 56 = 1L
      && Bitboard.get_nth whole 57 = 0L
      && Bitboard.get_nth whole 58 = 0L
      && Bitboard.get_nth whole 59 = 0L
      && Check.is_legal_move chessboard (Chessmove (K, 60, 60, None))
      && Check.is_legal_move chessboard (Chessmove (K, 60, 59, None))
      && Check.is_legal_move chessboard (Chessmove (K, 60, 58, None))
    then Board.LongCastling :: castling_moves
    else castling_moves
  else [] (* Le roi a deja joue, pas de roque possible *)
