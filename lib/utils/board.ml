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
    wrock: bool;
    brock: bool
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
    wrock = true;
    brock = true
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
    wrock = false;
    brock = false
};;

(* Renvoie le bitboard de l'ensemble des pieces enemies *)
let get_enemy_board chessboard =
  if chessboard.iswhite then
    List.fold_left logor 0L [
      chessboard.bpawns;
      chessboard.bknights;
      chessboard.bbishops;
      chessboard.brooks;
      chessboard.bqueen;
      chessboard.bking
    ]
  else
    List.fold_left logor 0L [
      chessboard.wpawns;
      chessboard.wknights;
      chessboard.wbishops;
      chessboard.wrooks;
      chessboard.wqueen;
      chessboard.wking
    ]
;;

(* Renvoie le bitboard de l'ensemble des pieces amies *)
let get_ally_board chessboard =
  if chessboard.iswhite then
    List.fold_left logor 0L [
      chessboard.wpawns;
      chessboard.wknights;
      chessboard.wbishops;
      chessboard.wrooks;
      chessboard.wqueen;
      chessboard.wking
    ]
  else
    List.fold_left logor 0L [
      chessboard.bpawns;
      chessboard.bknights;
      chessboard.bbishops;
      chessboard.brooks;
      chessboard.bqueen;
      chessboard.bking
    ]
;;

(* Renvoie le bitboard de l'ensemble des pieces de l'echiquier *)
let get_whole_board chessboard =
  logor (get_ally_board chessboard) (get_enemy_board chessboard)
;;

(* Renvoie *)
let if_w_else chessboard a b =
  match chessboard.iswhite with
  |true -> a
  |_ -> b
;;
