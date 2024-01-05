open Int64

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
  iswhite : bool;
  wcastle : bool;
  bcastle : bool;
}

(* Notation internationale des pieces, sans la couleur associé *)
type chesspiece = P | B | N | R | Q | K

(* Piece qui bouge / case de depart / case d'arrivee *)
type chessmove =
  | Chessmove of chesspiece * int * int
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
    iswhite = true;
    wcastle = true;
    bcastle = true;
  }

(* Position vide *)
let empty_board =
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
    iswhite = true;
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
    iswhite = true;
    wcastle = false;
    bcastle = false;
  }

(* Renvoie a si c'est au blanc de jouer, b sinon *)
let if_w_else board a b = match board.iswhite with true -> a | _ -> b

(* Change le trait *)
let change_turn board = { board with iswhite = not board.iswhite }

(* Renvoie le bitboard associé à la piece *)
let get_bitboard board = function
  | P -> if_w_else board board.wpawns board.bpawns
  | B -> if_w_else board board.wbishops board.bbishops
  | N -> if_w_else board board.wknights board.bknights
  | R -> if_w_else board board.wrooks board.brooks
  | Q -> if_w_else board board.wqueen board.bqueen
  | K -> if_w_else board board.wking board.bking

(* Renvoie le bitboard de l'ensemble des pieces amies *)
let get_ally_board board =
  List.fold_left logor 0L
    [
      get_bitboard board P;
      get_bitboard board K;
      get_bitboard board N;
      get_bitboard board R;
      get_bitboard board Q;
      get_bitboard board K;
    ]

(* Renvoie le bitboard de l'ensemble des pieces ennemies *)
let get_enemy_board board = get_ally_board (change_turn board)

(* Renvoie le bitboard de l'ensemble des pieces de l'echiquier *)
let get_whole_board board = logor (get_ally_board board) (get_enemy_board board)

(* Print l'echiquier complet *)
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
    (" - - - - - - - -\n" ^ print_board_aux 7 "" ^ " - - - - - - - -")

(* Modifie le bitboard associé à la piece p *)
let update_board board b = function
  | P ->
      {
        board with
        wpawns = if_w_else board b board.wpawns;
        bpawns = if_w_else board board.bpawns b;
      }
  | B ->
      {
        board with
        wbishops = if_w_else board b board.wbishops;
        bbishops = if_w_else board board.bbishops b;
      }
  | N ->
      {
        board with
        wknights = if_w_else board b board.wknights;
        bknights = if_w_else board board.bknights b;
      }
  | R ->
      {
        board with
        wrooks = if_w_else board b board.wrooks;
        brooks = if_w_else board board.brooks b;
      }
  | Q ->
      {
        board with
        wqueen = if_w_else board b board.wqueen;
        bqueen = if_w_else board board.bqueen b;
      }
  | K ->
      {
        board with
        wking = if_w_else board b board.wking;
        bking = if_w_else board board.bking b;
      }

(* Renvoie l'ensemble des positions possibles en ajoutant une piece précise *)
let add_piece board piece =
  let rec aux board piece acc = function
    | i when i > 63 -> List.map (fun b -> update_board board b piece) acc
    | i when Bitboard.get_nth (get_whole_board board) i = 1L ->
        aux board piece acc (i + 1)
    | i ->
        aux board piece
          (Bitboard.set_nth (get_bitboard board piece) i :: acc)
          (i + 1)
  in
  aux board piece [] 0
