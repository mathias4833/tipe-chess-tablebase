open Bigarray
open Utils

let open_table pieces =
  let size = 462 * (1 lsl (6 * List.length pieces)) * 2 in
  let file_descr =
    Unix.openfile "endgame.table"
      [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC ]
      0o644
  in
  let table = Unix.map_file file_descr int8_unsigned c_layout true [| size |] in
  Genarray.fill table 0;
  (file_descr, array1_of_genarray table)

let close_table file_descr = Unix.close file_descr

(* Associe chaque piece a un entier, pour avoir un ordre *)
let piece_to_number = function
  | Board.K, Board.White -> 0
  | Board.K, Board.Black -> 1
  | Board.Q, Board.White -> 2
  | Board.Q, Board.Black -> 3
  | Board.R, Board.White -> 4
  | Board.R, Board.Black -> 5
  | Board.B, Board.White -> 6
  | Board.B, Board.Black -> 7
  | Board.N, Board.White -> 8
  | Board.N, Board.Black -> 9
  | Board.P, Board.White -> 10
  | Board.P, Board.Black -> 11

let number_to_piece = function
  | 0 -> (Board.K, Board.White)
  | 1 -> (Board.K, Board.Black)
  | 2 -> (Board.Q, Board.White)
  | 3 -> (Board.Q, Board.Black)
  | 4 -> (Board.R, Board.White)
  | 5 -> (Board.R, Board.Black)
  | 6 -> (Board.B, Board.White)
  | 7 -> (Board.B, Board.Black)
  | 8 -> (Board.N, Board.White)
  | 9 -> (Board.N, Board.Black)
  | 10 -> (Board.P, Board.White)
  | 11 -> (Board.P, Board.Black)
  | _ -> failwith "Indice de la piece invalide"

let color_to_number = function Board.White -> 1 | Black -> 0

let number_to_color = function
  | 1 -> Board.White
  | 0 -> Board.Black
  | _ -> failwith "Indice invalide"

let white_king_to_number = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 9 -> 4
  | 10 -> 5
  | 11 -> 6
  | 18 -> 7
  | 19 -> 8
  | 27 -> 9
  | _ -> failwith "Indice invalide"

let number_to_white_king = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> 9
  | 5 -> 10
  | 6 -> 11
  | 7 -> 18
  | 8 -> 19
  | 9 -> 27
  | _ -> failwith "Indice invalide"

(* Associe les deux rois a un indice entre 0 et 461 *)
let kings_lookup_table =
  let index_of_couple = Array.make_matrix 10 64 (-1) in
  let couple_of_index = Array.make 462 (-1, -1) in
  let c = ref 0 in
  for p = 0 to 9 do
    let n = number_to_white_king p in
    let i, j = Bitboard.coord_of_index n in
    for m = 0 to 63 do
      let k, l = Bitboard.coord_of_index m in
      (* Si les rois ne se touchent pas et si le roi noir et sous la diagonale quand le roi blanc est dessus*)
      if (abs (i - k) > 1 || abs (j - l) > 1) && (i <> j || k <= l) then (
        index_of_couple.(p).(m) <- !c;
        couple_of_index.(!c) <- (n, m);
        incr c)
    done
  done;
  (index_of_couple, couple_of_index)

let kings_index_of_couple i j =
  let n = white_king_to_number i in
  (fst kings_lookup_table).(n).(j)

let kings_couple_of_index n = (snd kings_lookup_table).(n)

(* Renvoie un indice entre 0 et 2^(9 + pieces*6 + 1) *)
let board_to_number (board : Board.chessboard) pieces =
  let rec aux acc king_square = function
    | [] -> (2 * acc) + color_to_number board.color
    | h :: t ->
        let b = Board.get_bitboard board h in
        let n = if b = 0L then king_square else Bitboard.get_lsb b in
        aux ((acc * 64) + n) king_square t
  in
  let n = Bitboard.get_lsb (Board.get_bitboard board (Board.K, Board.White)) in
  let m = Bitboard.get_lsb (Board.get_bitboard board (Board.K, Board.Black)) in
  aux (kings_index_of_couple n m) n pieces

let number_to_board num pieces =
  let rec add_pieces acc num king_square = function
    | [] -> acc
    | h :: t ->
        let q = num / 64 in
        let r = num mod 64 in
        if r = king_square then add_pieces acc q king_square t
        else
          let b = Bitboard.set_nth 0L r in
          add_pieces (Board.set_bitboard acc b h) q king_square t
  in
  let color = number_to_color (num mod 2) in
  let kings_index = num lsr (1 + (6 * List.length pieces)) in
  let n, m = kings_couple_of_index kings_index in
  let board = Board.empty_board color in
  add_pieces
    (Board.set_bitboard
       (Board.set_bitboard board (Bitboard.set_nth 0L n) (Board.K, Board.White))
       (Bitboard.set_nth 0L m) (Board.K, Board.Black))
    (num / 2) n (List.rev pieces)
