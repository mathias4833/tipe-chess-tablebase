open Moves;;

(*
let l = Pawn.generate_moves (Board.init_board) in
Bitboard.print_list_board l;;
*)

(*
let (_, l) = Bishop.generate_blockers 0 0 in

let rec aux l =
  match l with
  |[] -> ()
  |(a, l2)::t -> (
    print_endline "--------";
    Bitboard.print_board a;
    print_endline "-";
    Bitboard.print_list_board l2;
    aux t
  )
in aux l;;
*)


let x = Rook.generate_possible_cases () in
print_endline (string_of_int (Hashtbl.length x.(0)));;


(*
let (x, _) = Rook.generate_magic (Rook.generate_blockers 3 3) in
print_endline (Int64.to_string x);;
*)
(*
let x = Attacks.generate_rook_attacks () in
let a, l = match x.(0) with
|(_, [])|(_, [_])|(_, [_; _])|(_, [_; _; _])|(_, [_; _; _; _])|(_, [_; _; _; _; _]) -> 0L, []
|(_, _::_::_::_::(a, b)::_) -> a, b
in
Board.print_bitboard a;
let rec aux l =
  print_endline "";
  match l with
  |[] -> print_endline "hey"; ()
  |h::t -> Board.print_bitboard h; aux t
in aux l
*)
(*
let x = Attacks.generate_all_magics () in
print_endline (Int64.to_string x.(0));;
*)

(* print_int (Coefs.close_king (2,1) (7,4));; *)

