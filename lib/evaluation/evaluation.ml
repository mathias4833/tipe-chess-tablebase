
(*
let diff_materiel chessboard =
1*((Utils.count_ones chessboard.w_pawns) - (Utils.count_ones chessboard.b_pawns))
+3*((Utils.count_ones chessboard.w_bishops) - (Utils.count_ones chessboard.b_bishops))
+3*((Utils.count_ones chessboard.w_knights) - (Utils.count_ones chessboard.b_knights))
+5*((Utils.count_ones chessboard.w_rooks) - (Utils.count_ones chessboard.b_rooks))
+9*((Utils.count_ones chessboard.w_queen) - (Utils.count_ones chessboard.b_queen))
;;

let point_value index chessboard =
  let rec point_value_aux i bit_chessboard =
    print_endline "1";
    match bit_chessboard with
    |0L -> 0
    |_ -> (
      let next_board = Utils.pop_lsb bit_chessboard in
      let next_i = Utils.get_lsb2 bit_chessboard in
      Coefs.w_pawn.(i) + (point_value_aux next_i next_board)
    )
  in point_value_aux index chessboard
;;

*)
