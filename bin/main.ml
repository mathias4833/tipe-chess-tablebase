open Moves;;
open Utils;;
(*
let l = Pawn.generate_moves (Board.init_board) in
Bitboard.print_list_board l;;
*)

(*
let l = Queen.generate_blockers 4 3  in

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
(*
print_int (Hashtbl.length (Bishop.table_moves.(Bitboard.index_of_coord 2 3)));
*)

(* Board.print_board (Board.study_board);; *)

let study_board_black = {Board.study_board with iswhite = false};;

Move.print_moves (study_board_black) (Rules.generate_moves study_board_black);;

print_int (List.length (Rules.generate_moves study_board_black));;

