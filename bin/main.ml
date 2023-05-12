open Chess_engine;;

let x = Bishop.generate_blockers 3 5 in
Utils.print_list_board x;;
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

