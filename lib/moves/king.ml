open Int64
open Utils

(** [table_mask] crée une table de bitboards représentant les cases attaquées par un roi depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_move =
  let create_table_move n =
    let i, j = Bitboard.coord_of_index n in
    Bitboard.from_coordinates
      [
        (i, j - 1);
        (i + 1, j - 1);
        (i + 1, j);
        (i + 1, j + 1);
        (i, j + 1);
        (i - 1, j + 1);
        (i - 1, j);
        (i - 1, j - 1);
      ]
  in
  Array.init 64 create_table_move

(** [generate_moves chessboard] génère les coups possibles pour le roi.
    @param chessboard Le plateau d'échecs.
    @return Liste des coups possibles pour le roi. *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  (* Indice du roi sur l'echiquier *)
  let n = Bitboard.get_lsb (Board.get_ally_bitboard chessboard K) in
  Move.add_moves_to_list K n (logand table_move.(n) (lognot ally)) []

(** [generate_unmoves chessboard pieces] génère les annulations de coups possibles pour le roi.
    @param chessboard Le plateau d'échecs.
    @param pieces Liste des pieces présentes sur le plateau.
    @return Liste des annulations de coups possibles pour le roi. *)
let generate_unmoves chessboard pieces =
  let whole = Board.get_whole_board chessboard in
  let n = Bitboard.get_lsb (Board.get_ally_bitboard chessboard K) in
  Move.add_unmoves_to_list K n [] chessboard pieces
    (logand table_move.(n) (lognot whole))
