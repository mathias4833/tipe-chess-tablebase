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
    b_king: int64
};;

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
};;

(* Print the bitboard *)
(* TODO: Commenter + fix echiquier inverse *)
let print_bitboard x =
  let rec aux i n =
    match (i, n) with
    |i, _ when i > (64 + 8) -> ""
    |i, _ when i mod 9 = 0 -> (aux (i+1) n) ^ "\n"
    |_, n when (logand n 1L) = 1L -> (aux (i+1) (shift_right n 1)) ^ "1"
    |_ -> (aux (i+1) (shift_right n 1)) ^ "."
  in print_string (aux 0 x)
;;