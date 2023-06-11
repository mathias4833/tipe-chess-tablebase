open Int64;;

type chessboard = {
    wpawns: int64;
    wknights: int64;
    wbishops: int64;
    wrooks: int64;
    wqueen: int64;
    wking: int64;
    bpawns: int64;
    bknights: int64;
    bbishops: int64;
    brooks: int64;
    bqueen: int64;
    bking: int64;
    iswhite: bool;
    wcastle: bool;
    bcastle: bool
};;

(* Position initiale *)
let init_board = {
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
    bcastle = true
};;

(* Position d'etude *)
let study_board = {
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
    bcastle = false
};;

(* Renvoie le bitboard de l'ensemble des pieces enemies *)
let get_enemy_board board =
  if board.iswhite then
    List.fold_left logor 0L [
      board.bpawns;
      board.bknights;
      board.bbishops;
      board.brooks;
      board.bqueen;
      board.bking
    ]
  else
    List.fold_left logor 0L [
      board.wpawns;
      board.wknights;
      board.wbishops;
      board.wrooks;
      board.wqueen;
      board.wking
    ]
;;

(* Renvoie le bitboard de l'ensemble des pieces amies *)
let get_ally_board board =
  if board.iswhite then
    List.fold_left logor 0L [
      board.wpawns;
      board.wknights;
      board.wbishops;
      board.wrooks;
      board.wqueen;
      board.wking
    ]
  else
    List.fold_left logor 0L [
      board.bpawns;
      board.bknights;
      board.bbishops;
      board.brooks;
      board.bqueen;
      board.bking
    ]
;;

(* Renvoie le bitboard de l'ensemble des pieces de l'echiquier *)
let get_whole_board board =
  logor (get_ally_board board) (get_enemy_board board)
;;

(* Renvoie *)
let if_w_else board a b =
  match board.iswhite with
  |true -> a
  |_ -> b
;;

(* Print l'echiquier complet *)
let print_board board =
  let rec print_case i j =
    let n = Bitboard.index_of_coord i j in
    match n with
    |n when (Bitboard.get_nth board.wpawns n) = 1L -> "♟︎" 
    |n when (Bitboard.get_nth board.wknights n) = 1L -> "♞"
    |n when (Bitboard.get_nth board.wbishops n) = 1L -> "♝"
    |n when (Bitboard.get_nth board.wrooks n) = 1L -> "♜"
    |n when (Bitboard.get_nth board.wqueen n) = 1L -> "♛"
    |n when (Bitboard.get_nth board.wking n) = 1L -> "♚"
    |n when (Bitboard.get_nth board.bpawns n) = 1L -> "♙"
    |n when (Bitboard.get_nth board.bknights n) = 1L -> "♘"
    |n when (Bitboard.get_nth board.bbishops n) = 1L -> "♗"
    |n when (Bitboard.get_nth board.brooks n) = 1L -> "♖"
    |n when (Bitboard.get_nth board.bqueen n) = 1L -> "♕"
    |n when (Bitboard.get_nth board.bking n) = 1L -> "♔"
    |_ -> " "
  (* Print la ligne i de l'echiquier *)
  and print_line i acc =
    let rec aux j acc =
      match j with
      |j when j > 7 -> acc ^ "|\n"
      |_ -> aux (j+1) (acc ^ "|" ^ (print_case i j))
    in aux 0 acc
  (* Print l'echiquier ligne par ligne *)
  and print_board_aux i acc =
    match i with
    |i when i < 0 -> acc
    |_ -> print_board_aux (i-1) (print_line i acc)
  in print_endline (" - - - - - - - -\n" ^ (print_board_aux 7 "") ^ " - - - - - - - -")
;;
