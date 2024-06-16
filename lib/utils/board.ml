open Int64

type color = White | Black
type chesspiece = P | B | N | R | Q | K
type colored_chesspiece = chesspiece * color

type chessboard = {
  wpawns : int64;
  wknights : int64;
  wbishops : int64;
  wrooks : int64;
  wqueen : int64;
  wking : int64;
  bpawns : int64;
  bknights : int64;
  bbishops : int64;
  brooks : int64;
  bqueen : int64;
  bking : int64;
  color : color;
  wcastle : bool;
  bcastle : bool;
}

(* Piece qui bouge / case de depart / case d'arrivee / piece mangee *)
type chessmove =
  | Chessmove of chesspiece * int * int * colored_chesspiece option
  | ShortCastling
  | LongCastling

(* Position initiale *)
let init_board =
  {
    wpawns = 0xff00L;
    wknights = 0x42L;
    wbishops = 0x24L;
    wrooks = 0x81L;
    wqueen = 0x8L;
    wking = 0x10L;
    bpawns = 0xff000000000000L;
    bknights = 0x4200000000000000L;
    bbishops = 0x2400000000000000L;
    brooks = 0x8100000000000000L;
    bqueen = 0x800000000000000L;
    bking = 0x1000000000000000L;
    color = White;
    wcastle = true;
    bcastle = true;
  }

(* Position vide *)
let empty_board color =
  {
    wpawns = 0L;
    wknights = 0L;
    wbishops = 0L;
    wrooks = 0L;
    wqueen = 0L;
    wking = 0L;
    bpawns = 0L;
    bknights = 0L;
    bbishops = 0L;
    brooks = 0L;
    bqueen = 0L;
    bking = 0L;
    color;
    wcastle = false;
    bcastle = false;
  }

(* Position d'etude *)
let study_board =
  {
    wpawns = 0x20000044800700L;
    wknights = 0x100000L;
    wbishops = 0x80000L;
    wrooks = 0x4000000000000004L;
    wqueen = 0x800000000L;
    wking = 0x2L;
    bpawns = 0x45800200000000L;
    bknights = 0x400000200000L;
    bbishops = 0x0L;
    brooks = 0x20000000040L;
    bqueen = 0x10000000000L;
    bking = 0x80000000000000L;
    color = White;
    wcastle = false;
    bcastle = false;
  }

(** [is_white board] vérifie si le joueur associé au plateau de jeu est blanc.
    @param board Le plateau de jeu à vérifier.
    @return true si le joueur est blanc, false sinon. *)
let is_white board = board.color = White

(** [change_color color] inverse la couleur donnée.
    @param color La couleur à inverser.
    @return La couleur opposée à celle donnée. *)
let change_color = function White -> Black | Black -> White

(** [if_w_else board a b] exécute l'expression [a] si la couleur du plateau est blanche, sinon exécute l'expression [b].
    @param board Le plateau de jeu.
    @param a Expression à exécuter si la couleur est blanche.
    @param b Expression à exécuter si la couleur n'est pas blanche.
    @return Résultat de l'expression exécutée. *)
let if_w_else board a b = match board.color with White -> a | _ -> b

(** [change_turn board] change la couleur du plateau pour passer au tour du joueur suivant.
    @param board Le plateau de jeu actuel.
    @return Le plateau de jeu avec la couleur mise à jour pour le tour suivant. *)
let change_turn board = { board with color = change_color board.color }

(** [get_bitboard board piece_color] récupère le bitboard correspondant à la couleur et au type de pièce spécifiés sur le plateau donné.
    @param board Le plateau de jeu.
    @param piece_color La couleur de la pièce (White ou Black).
    @return Le bitboard correspondant à la couleur et au type de pièce spécifiés. *)
let get_bitboard board = function
  | P, White -> board.wpawns
  | B, White -> board.wbishops
  | N, White -> board.wknights
  | R, White -> board.wrooks
  | Q, White -> board.wqueen
  | K, White -> board.wking
  | P, Black -> board.bpawns
  | B, Black -> board.bbishops
  | N, Black -> board.bknights
  | R, Black -> board.brooks
  | Q, Black -> board.bqueen
  | K, Black -> board.bking

(** [get_piece_positions board pieces] retourne la position de chaque occurrence des pièces données.
    Les occurrences identiques sont associées aux bits par ordre croissant.
    @param board Le plateau d'échecs.
    @param pieces La liste des pièces dont on cherche les positions.
    @return La liste des positions, avec [None] pour chaque pièce absente. *)
let get_piece_positions board pieces =
  let rec aux remaining acc = function
    | [] -> List.rev acc
    | piece :: tail ->
        let bitboard =
          match List.assoc_opt piece remaining with
          | Some bitboard -> bitboard
          | None -> get_bitboard board piece
        in
        let position, next_bitboard =
          if bitboard = 0L then (None, 0L)
          else
            ( Some (Bitboard.get_lsb bitboard),
              Bitboard.pop_lsb bitboard )
        in
        let remaining =
          (piece, next_bitboard) :: List.remove_assoc piece remaining
        in
        aux remaining (position :: acc) tail
  in
  aux [] [] pieces

(** [get_ally_bitboard board piece] récupère le bitboard des pièces alliées du type spécifié sur le plateau donné.
    @param board Le plateau de jeu.
    @param piece Le type de pièce (P, B, N, R, Q, K).
    @return Le bitboard des pièces alliées du type spécifié. *)
let get_ally_bitboard board piece = get_bitboard board (piece, board.color)

(** [get_enemy_bitboard board piece] récupère le bitboard des pièces ennemies du type spécifié sur le plateau donné.
    @param board Le plateau de jeu.
    @param piece Le type de pièce (P, B, N, R, Q, K).
    @return Le bitboard des pièces ennemies du type spécifié. *)
let get_enemy_bitboard board piece =
  get_bitboard board (piece, change_color board.color)

(** [get_ally_board board] récupère le bitboard de toutes les pièces alliées sur le plateau donné.
    @param board Le plateau de jeu.
    @return Le bitboard de toutes les pièces alliées. *)
let get_ally_board board =
  List.fold_left logor 0L
    [
      get_ally_bitboard board P;
      get_ally_bitboard board B;
      get_ally_bitboard board N;
      get_ally_bitboard board R;
      get_ally_bitboard board Q;
      get_ally_bitboard board K;
    ]

(** [get_enemy_board board] récupère le bitboard de toutes les pièces ennemies sur le plateau donné.
    @param board Le plateau de jeu.
    @return Le bitboard de toutes les pièces ennemies. *)
let get_enemy_board board = get_ally_board (change_turn board)

(** [get_whole_board board] récupère le bitboard de toutes les pièces sur le plateau donné.
    @param board Le plateau de jeu.
    @return Le bitboard de toutes les pièces présentes sur le plateau. *)
let get_whole_board board = logor (get_ally_board board) (get_enemy_board board)

(** [print_board board] affiche un plateau de jeu.
    @param board le plateau de jeu à afficher. *)
let print_board board =
  let rec print_case i j =
    let n = Bitboard.index_of_coord i j in
    match n with
    | n when Bitboard.get_nth board.wpawns n = 1L -> "♟︎"
    | n when Bitboard.get_nth board.wknights n = 1L -> "♞"
    | n when Bitboard.get_nth board.wbishops n = 1L -> "♝"
    | n when Bitboard.get_nth board.wrooks n = 1L -> "♜"
    | n when Bitboard.get_nth board.wqueen n = 1L -> "♛"
    | n when Bitboard.get_nth board.wking n = 1L -> "♚"
    | n when Bitboard.get_nth board.bpawns n = 1L -> "♙"
    | n when Bitboard.get_nth board.bknights n = 1L -> "♘"
    | n when Bitboard.get_nth board.bbishops n = 1L -> "♗"
    | n when Bitboard.get_nth board.brooks n = 1L -> "♖"
    | n when Bitboard.get_nth board.bqueen n = 1L -> "♕"
    | n when Bitboard.get_nth board.bking n = 1L -> "♔"
    | _ -> " "
  (* Print la ligne i de l'echiquier *)
  and print_line i acc =
    let rec aux j acc =
      match j with
      | j when j > 7 -> acc ^ "|\n"
      | _ -> aux (j + 1) (acc ^ "|" ^ print_case i j)
    in
    aux 0 acc
  (* Print l'echiquier ligne par ligne *)
  and print_board_aux i acc =
    match i with
    | i when i < 0 -> acc
    | _ -> print_board_aux (i - 1) (print_line i acc)
  in
  print_endline
    (" - - - - - - - -\n" ^ print_board_aux 7 "" ^ " - - - - - - - -");
  flush stdout

(** [print_list_board l] affiche une liste d'echiquiers.
    @param l La liste d'echiquier à afficher. *)
let rec print_list_board l =
  match l with
  | [] -> ()
  | h :: t ->
      print_board h;
      print_string "\n";
      print_list_board t

(** [set_bitboard board bitboard piece color] définit le bitboard des pièces d'un certain type et couleur.
    @param board Le plateau de jeu.
    @param bitboard Le bitboard à définir.
    @param piece Le type de pièce.
    @param color La couleur de la pièce.
    @return Le plateau de jeu avec le bitboard mis à jour. *)
let set_bitboard board b = function
  | P, White -> { board with wpawns = b }
  | B, White -> { board with wbishops = b }
  | N, White -> { board with wknights = b }
  | R, White -> { board with wrooks = b }
  | Q, White -> { board with wqueen = b }
  | K, White -> { board with wking = b }
  | P, Black -> { board with bpawns = b }
  | B, Black -> { board with bbishops = b }
  | N, Black -> { board with bknights = b }
  | R, Black -> { board with brooks = b }
  | Q, Black -> { board with bqueen = b }
  | K, Black -> { board with bking = b }

(** [add_piece board piece] ajoute une pièce de jeu donnée au plateau, générant toutes les configurations possibles résultantes.
    @param board Le plateau de jeu.
    @param piece La pièce de jeu à ajouter.
    @return La liste des plateaux de jeu obtenus après avoir ajouté la pièce donnée. *)
let add_piece board (piece : colored_chesspiece) =
  let rec aux acc = function
    | i when i > 63 -> List.map (fun b -> set_bitboard board b piece) acc
    | i when Bitboard.get_nth (get_whole_board board) i = 1L -> aux acc (i + 1)
    | i -> aux (Bitboard.set_nth (get_bitboard board piece) i :: acc) (i + 1)
  in
  aux [] 0
