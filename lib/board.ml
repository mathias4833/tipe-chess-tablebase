open Int64;;

type chessboard = {
    w_pawns: int64;
    w_knights: int64;
    w_bishops: int64;
    w_rooks: int64;
    w_queen: int64;
    w_king: int64;
    b_pawns: int64;
    b_knights: int64;
    b_bishops: int64;
    b_rooks: int64;
    b_queen: int64;
    b_king: int64;
    is_white: bool
};;

(* Layout *)
let layout = [|
  56;57;58;59;60;61;62;63;
  48;49;50;51;52;53;54;55;
  40;41;42;43;44;45;46;47;
  32;33;34;35;36;37;38;39;
  24;25;26;27;28;29;30;31;
  16;17;18;19;20;21;22;23;
  08;09;10;11;12;13;14;15;
  00;01;02;03;04;05;06;07;
|];;

let column = 0x101010101010101L;;
let line = 0xffL;;

(* Position initiale *)
let init_board = {
    w_pawns = 0xff00L;
    w_knights = 0x42L;
    w_bishops = 0x24L;
    w_rooks = 0x81L;
    w_queen = 0x8L;
    w_king = 0x10L;
    b_pawns = 0xff000000000000L;
    b_knights = 0x4200000000000000L;
    b_bishops = 0x2400000000000000L;
    b_rooks = 0x8100000000000000L;
    b_queen = 0x800000000000000L;
    b_king = 0x1000000000000000L;
    is_white = true
};;

(* Print the bitboard *)
let print_bitboard x =
  let rec line i n =
    match i, n with
    |(i, _) when i < 0 -> "\n"
    |(_, n) when (logand n 1L) = 1L -> "1" ^ (line (i-1) (shift_right n 1))
    |_ -> "." ^ (line (i-1) (shift_right n 1))
  and column i n =
    match (i, n) with
    |(i, _) when i < 0 -> ""
    |_ -> (column (i-1) (shift_right n 8)) ^ (line 7 n)
  in print_string (column 7 x)
;;