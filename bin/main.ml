open Chess_engine;;

(*
let masks = Attacks.generate_rook_masks;;
let m = Array.init 64 (fun i -> Attacks.generate_blockers masks.(i));;

Board.print_bitboard m.(20).(95);;
print_int ( Utils.get_lsb m.(20).(95));;*)


let i = Utils.get_lsb2 Board.init_board.w_pawns in
Board.print_bitboard Board.init_board.w_pawns;
let y = Evaluation.point_value i (Utils.pop_lsb Board.init_board.w_pawns) in
print_endline (string_of_int y);;



(* Coef_case.diff_materiel Board.init_board ;;*)