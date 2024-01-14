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

let is_white board = board.color = White
let change_color = function White -> Black | Black -> White

(* Renvoie a si c'est au blanc de jouer, b sinon *)
let if_w_else board a b = match board.color with White -> a | _ -> b

(* Change le trait *)
let change_turn board = { board with color = change_color board.color }

(* Renvoie le bitboard associé à la piece *)
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

let get_ally_bitboard board piece = get_bitboard board (piece, board.color)

let get_enemy_bitboard board piece =
  get_bitboard board (piece, change_color board.color)

(* Renvoie le bitboard de l'ensemble des pieces amies *)
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
    (" - - - - - - - -\n" ^ print_board_aux 7 "" ^ " - - - - - - - -");
  flush stdout

(* Modifie le bitboard associé à la piece p *)
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

(* Renvoie l'ensemble des positions possibles en ajoutant une piece précise *)
let add_piece board (piece : colored_chesspiece) =
  let rec aux acc = function
    | i when i > 63 -> List.map (fun b -> set_bitboard board b piece) acc
    | i when Bitboard.get_nth (get_whole_board board) i = 1L -> aux acc (i + 1)
    | i -> aux (Bitboard.set_nth (get_bitboard board piece) i :: acc) (i + 1)
  in
  aux [] 0

(* Associe chaque piece a un entier, pour avoir un ordre *)
let piece_to_number = function
  | K, White -> 0
  | K, Black -> 1
  | Q, White -> 2
  | Q, Black -> 3
  | R, White -> 4
  | R, Black -> 5
  | B, White -> 6
  | B, Black -> 7
  | N, White -> 8
  | N, Black -> 9
  | P, White -> 10
  | P, Black -> 11

let number_to_piece = function
  | 0 -> (K, White)
  | 1 -> (K, Black)
  | 2 -> (Q, White)
  | 3 -> (Q, Black)
  | 4 -> (R, White)
  | 5 -> (R, Black)
  | 6 -> (B, White)
  | 7 -> (B, Black)
  | 8 -> (N, White)
  | 9 -> (N, Black)
  | 10 -> (P, White)
  | 11 -> (P, Black)
  | _ -> failwith "Indice de la piece invalide"

let color_to_number = function White -> 1 | Black -> 0

let number_to_color = function
  | 1 -> White
  | 0 -> Black
  | _ -> failwith "Indice invalide"

(* let board_to_number b _ = b *)
(* let number_to_board n _ = n *)

let board_to_number board pieces =
  let rec aux acc prev_square = function
    | [] -> (2 * acc) + color_to_number board.color
    | h :: t ->
        let b = get_bitboard board h in
        let n = if b = 0L then prev_square else Bitboard.get_lsb b in
        aux ((acc * 64) + n) n t
  in
  aux 0 (-1) pieces

let number_to_board num pieces =
  let rec map_pieces_to_indices acc num = function
    | [] -> acc
    | h :: t -> map_pieces_to_indices ((h, num mod 64) :: acc) (num / 64) t
  in
  let rec create_board acc = function
    | [] -> acc
    | (p, n) :: (_, m) :: t when n = m -> create_board acc ((p, n) :: t)
    | (p, n) :: t ->
        let b = Bitboard.set_nth 0L n in
        create_board (set_bitboard acc b p) t
  in
  let color = number_to_color (num mod 2) in
  create_board (empty_board color)
    (map_pieces_to_indices [] (num / 2) (List.rev pieces))
