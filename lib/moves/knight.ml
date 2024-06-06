open Int64
open Utils

(** [table_move] crée une table de bitboards représentant les cases attaquées par un cavalier depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_move =
  let create_table_move n =
    let i, j = Bitboard.coord_of_index n in
    Bitboard.from_coordinates
      [
        (i + 1, j - 2);
        (i + 2, j - 1);
        (i + 2, j + 1);
        (i + 1, j + 2);
        (i - 1, j + 2);
        (i - 2, j + 1);
        (i - 2, j - 1);
        (i - 1, j - 2);
      ]
  in
  Array.init 64 create_table_move

(** [generate_moves chessboard] génère les coups possibles pour tous les cavaliers.
    @param chessboard Le plateau d'échecs.
    @return Liste des coups possibles pour les cavalier. *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let rec generate_moves_aux acc = function
    | 0L -> acc
    | board ->
        (* Indice du cavalier *)
        let n = Bitboard.get_lsb board in
        let all_moves = logand table_move.(n) (lognot ally) in
        generate_moves_aux
          (Move.add_moves_to_list N n all_moves acc)
          (Bitboard.pop_lsb board)
  in
  generate_moves_aux [] (Board.get_ally_bitboard chessboard N)

(** [generate_unmoves chessboard pieces] génère les annulations de coups possibles pour tous les cavaliers.
    @param chessboard Le plateau d'échecs.
    @param pieces Liste des pieces présentes sur le plateau.
    @return Liste des annulations de coups possibles pour les cavaliers. *)
let generate_unmoves chessboard pieces =
  let whole = Board.get_whole_board chessboard in
  let rec generate_unmoves_aux acc = function
    | 0L -> acc
    | b ->
        let n = Bitboard.get_lsb b in
        let all_unmoves = logand table_move.(n) (lognot whole) in
        generate_unmoves_aux
          (Move.add_unmoves_to_list N n acc chessboard pieces all_unmoves)
          (Bitboard.pop_lsb b)
  in
  generate_unmoves_aux [] (Board.get_ally_bitboard chessboard N)
