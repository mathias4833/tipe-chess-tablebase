open Chess_engine;;

let masks = Attacks.generate_rook_masks;;
let m = Array.init 64 (fun i -> Attacks.generate_blockers masks.(i));;
Board.print_bitboard m.(0).(1901);;









(* Coef_case.diff_materiel Board.init_board ;;*)