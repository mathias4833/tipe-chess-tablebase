open Moves;;
open Utils;;

(*
let l = Pawn.generate_moves (Board.init_board) in
Bitboard.print_list_board l;;
*)

(*
let (_, l) = Rook.generate_blockers 0 0 in

let rec aux l =
  match l with
  |[] -> ()
  |(a, l2)::t -> (
    print_endline "-->";
    Bitboard.print_board a;
    print_endline "<--";
    Bitboard.print_list_board l2;
    aux t
  )
in aux l;;
*)

Bitboard.print_list_board (Rook.generate_moves (Board.study_board));;

