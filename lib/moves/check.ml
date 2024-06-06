open Int64
open Utils

(** [is_legal board] vérifie si la position actuelle est légale, ie si le roi n'est pas menacé.
    @param board Le plateau d'échecs.
    @return [true] si la position est légale, [false] sinon. *)
let is_legal board =
  let enemy = Board.get_enemy_board board in
  let whole = Board.get_whole_board board in
  let n = Bitboard.get_lsb (Board.get_enemy_bitboard board Board.K) in

  (* Aucune tour ni dame n'attaque le roi sur les lignes et les colonnes *)
  logand
    (logor
       (Board.get_ally_bitboard board Board.Q)
       (Board.get_ally_bitboard board Board.R))
    (Rook.moves_from_board enemy whole n)
  = 0L
  (* Aucun fou ni dame n'attaque le roi en diagonale *)
  && logand
       (logor
          (Board.get_ally_bitboard board Board.Q)
          (Board.get_ally_bitboard board Board.B))
       (Bishop.moves_from_board enemy whole n)
     = 0L
  (* Aucun cavalier n'attaque le roi *)
  && logand (Board.get_ally_bitboard board Board.N) Knight.table_move.(n) = 0L
  (* Le roi adverse n'attaque pas le roi *)
  && logand (Board.get_ally_bitboard board Board.K) King.table_move.(n) = 0L
  && (* Aucun pion n'attaque le roi *)
  logand
    (Board.get_ally_bitboard board Board.P)
    (Board.if_w_else board Pawn.table_btake.(n) Pawn.table_wtake.(n))
  = 0L

(** [is_legal_move board move] vérifie si un coup est légal.
    @param board Le plateau d'échecs avant le coup.
    @param move Le coup à vérifier.
    @return [true] si le coup est légal, [false] sinon. *)
let is_legal_move board move = is_legal (Move.play_move board move)

(** [is_check board] vérifie si le roi du joueur actuel est en échec.
    @param board Le plateau d'échecs.
    @return [true] si le roi est en échec, [false] sinon. *)
let is_check (board : Board.chessboard) =
  (* On regarde si c'etait a l'adversaire de jouer s'il pourrait manger le roi*)
  not (is_legal (Board.change_turn board))
