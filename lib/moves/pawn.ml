open Int64
open Utils

(** [table_wtake] crée une table de bitboards représentant les cases attaquées par un pion blanc depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_wtake =
  let create_table_capture n =
    let i, j = Bitboard.coord_of_index n in
    Bitboard.from_coordinates [ (i + 1, j - 1); (i + 1, j + 1) ]
  in
  Array.init 64 create_table_capture

(** [table_btake] crée une table de bitboards représentant les cases attaquées par un pion noir depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_btake =
  let create_table_capture n =
    let i, j = Bitboard.coord_of_index n in
    Bitboard.from_coordinates [ (i - 1, j - 1); (i - 1, j + 1) ]
  in
  Array.init 64 create_table_capture

(** [table_wmove] crée une table de bitboards représentant les cases accessible en avancant par un pion blanc depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_wmove =
  let create_table_move n =
    let i, j = Bitboard.coord_of_index n in
    Bitboard.from_coordinate (i + 1) j
  in
  Array.init 64 create_table_move

(** [table_bmove] crée une table de bitboards représentant les cases accessible en avancant par un pion noir depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_bmove =
  let create_table_move n =
    let i, j = Bitboard.coord_of_index n in
    Bitboard.from_coordinate (i - 1) j
  in
  Array.init 64 create_table_move

(** [get_move_board color whole n] génère les coups possibles pour une case donnee.
    @param color La couleur du pion (White ou Black).
    @param whole Bitboard des pièces sur tout le plateau.
    @param n Indice de la position du pion sur l'échiquier (entre 0 et 63).
    @return Bitboard des coups possibles pour la pièce. *)
let get_move_board color whole n =
  let moveboard =
    match color with
    | Board.White ->
        let defaultmove = table_wmove.(n) in
        (* Verifie si le pion blanc peut avancer de deux cases *)
        if 7 < n && n < 16 && logand defaultmove whole = 0L then
          logor defaultmove table_wmove.(n + 8)
        else defaultmove
    | Board.Black ->
        let defaultmove = table_bmove.(n) in
        (* Verifie si le pion noir peut avancer de deux cases *)
        if 47 < n && n < 56 && logand defaultmove whole = 0L then
          logor defaultmove table_bmove.(n - 8)
        else defaultmove
  in
  logand moveboard (lognot whole)

(** [generate_moves chessboard] génère les coups possibles pour tous les pions.
    @param chessboard Le plateau d'échecs.
    @return Liste des coups possibles pour les pions. *)
let generate_moves chessboard =
  let enemy = Board.get_enemy_board chessboard in
  let whole = Board.get_whole_board chessboard in

  let rec generate_moves_aux board acc =
    match board with
    | 0L -> acc
    | _ ->
        (* Indice du pion *)
        let n = Bitboard.get_lsb board in
        (* Bitboards des coups par prise / sans prise en fonction de la couleur *)
        let takeboard =
          Board.if_w_else chessboard table_wtake.(n) table_btake.(n)
        in
        let moveboard = get_move_board chessboard.color whole n in

        (* Bitboard resultant des coups possibles *)
        let all_moves = logor (logand takeboard enemy) moveboard in

        (* Verifie si il y a promotion *)
        if Board.if_w_else chessboard (n > 47) (n < 16) then
          generate_moves_aux (Bitboard.pop_lsb board)
            (Move.add_moves_to_list Q n all_moves
               (Move.add_moves_to_list R n all_moves
                  (Move.add_moves_to_list B n all_moves
                     (Move.add_moves_to_list N n all_moves acc))))
        else
          generate_moves_aux (Bitboard.pop_lsb board)
            (Move.add_moves_to_list P n all_moves acc)
  in
  generate_moves_aux
    (Board.if_w_else chessboard chessboard.wpawns chessboard.bpawns)
    []
