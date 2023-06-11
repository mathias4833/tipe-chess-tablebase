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

(* Genere les coups de roque si possible *)
let generate_castling_moves (chessboard: Board.chessboard) =
  if chessboard.iswhite then
    (* Le roi n'a pas encore bouge *)
    if chessboard.wcastle then (
      let whole = Board.get_whole_board chessboard in
      (* Verifie que la tour est a sa place et qu'aucune piece ne bloque et que le roi ne se met pas en echecs *)
      let castling_moves =
        (* Petit roque *)
        if (Bitboard.get_nth chessboard.wrooks 7) = 1
          && (Bitboard.get_nth whole 5) = 1
          && (Bitboard.get_nth whole 6) = 1
          && (Rules.is_legal_move chessboard Move.chessmove(K, 4, 5))
          && (Rules.is_legal_move chessboard Move.chessmove(K, 4, 6))
        then [Move.ShortCastling] else [] in
      (* Grand roque *)
      if (Bitboard.get_nth chessboard.wrooks 0) = 1
        && (Bitboard.get_nth whole 1) = 1
        && (Bitboard.get_nth whole 2) = 1
        && (Bitboard.get_nth whole 3) = 1
        && (Rules.is_legal_move chessboard Move.chessmove(K, 4, 1))
        && (Rules.is_legal_move chessboard Move.chessmove(K, 4, 2))
        && (Rules.is_legal_move chessboard Move.chessmove(K, 4, 3))
      then (Move.LongCastling::castling_moves) else castling_moves
    )
  else (
    (* Le roi n'a pas encore bouge *)
    if chessboard.bcastle then (
      let whole = Board.get_whole_board chessboard in
      (* Verifie que la tour est a sa place et qu'aucune piece ne bloque et que le roi ne se met pas en echecs *)
      let castling_moves =
        (* Petit roque *)
        if (Bitboard.get_nth chessboard.brooks 63) = 1
          && (Bitboard.get_nth whole 61) = 1
          && (Bitboard.get_nth whole 62) = 1
          && (Rules.is_legal_move chessboard Move.chessmove(K, 60, 61))
          && (Rules.is_legal_move chessboard Move.chessmove(K, 60, 61))
        then [Move.ShortCastling] else [] in
      (* Grand roque *)
      if (Bitboard.get_nth chessboard.wrooks 56) = 1
        && (Bitboard.get_nth whole 57) = 1
        && (Bitboard.get_nth whole 58) = 1
        && (Bitboard.get_nth whole 59) = 1
        && (Rules.is_legal_move chessboard Move.chessmove(K, 60, 57))
        && (Rules.is_legal_move chessboard Move.chessmove(K, 60, 58))
        && (Rules.is_legal_move chessboard Move.chessmove(K, 60, 59))
      then (Move.LongCastling::castling_moves) else castling_moves
    )
  )
;;

(* Genere l'ensemble des coups pour le roi *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  (* Indice du roi sur l'echiquier *)
  let n = Bitboard.get_lsb (Board.if_w_else chessboard chessboard.wking chessboard.bking) in

  Move.add_moves_to_list K n (logand table_move.(n) (lognot ally)) (generate_castling_moves chessboard);
;;
