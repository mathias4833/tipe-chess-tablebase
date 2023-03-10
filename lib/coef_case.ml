
let diff_materiel (chessboard: Board.chessboard) =
1*((Utils.count_ones chessboard.w_pawns) - (Utils.count_ones chessboard.b_pawns))
+3*((Utils.count_ones chessboard.w_bishops) - (Utils.count_ones chessboard.b_bishops))
+3*((Utils.count_ones chessboard.w_knights) - (Utils.count_ones chessboard.b_knights))
+5*((Utils.count_ones chessboard.w_rooks) - (Utils.count_ones chessboard.b_rooks))
+9*((Utils.count_ones chessboard.w_queen) - (Utils.count_ones chessboard.b_queen))
;;
(*
let point_value chessboard=
  let aux_value piece index 
  match piece with
  |w_king -> 100000*coef_w_king.(index)
  |b_king -> 100000*coef_b_king.(index)
  |w_pawn
  

;;





let coef_w_king = ref [|0.2;0.2;0.2;0.2;0.2;0.2;0.2;0.2
                      0.2;0.2;0.2;0.2;0.2;0.2;0.2;0.2
                      0.2;0.2;0.2;0.2;0.2;0.2;0.2;0.2
                      0.2;0.2;0.2;0.1;0.1;0.2;0.2;0.2
                      0.3;0.3;0.3;0.1;0.1;0.3;0.3;0.3
                      0.5;0.5;0.4;0.4;0.4;0.4;0.5;0.5
                      0.8;0.8;0.8;0.7;0.7;0.8;0.8;0.8
                      1.0;1.0;0.9;0.8;0.8;0.9;1.0;1.0|] 
                    
let coef_b_king = ref [|1.0;1.0;0.9;0.8;0.8;0.9;1.0;1.0
                      0.8;0.8;0.8;0.7;0.7;0.8;0.8;0.8
                      0.5;0.5;0.4;0.4;0.4;0.4;0.5;0.5
                      0.3;0.3;0.3;0.1;0.1;0.3;0.3;0.3
                      0.2;0.2;0.2;0.1;0.1;0.2;0.2;0.2
                      0.2;0.2;0.2;0.2;0.2;0.2;0.2;0.2
                      0.2;0.2;0.2;0.2;0.2;0.2;0.2;0.2
                      0.2;0.2;0.2;0.2;0.2;0.2;0.2;0.2|] in                     

let coef_w_pawn = ref [|0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0
                        0.4;0.4;0.4;0.5;0.5;0.4;0.4;0.4
                        0.4;0.4;0.5;0.6;0.6;0.5;0.4;0.4
                        0.6;0.6;0.8;1.0;1.0;0.8;0.6;0.6
                        0.6;0.6;0.8;1.0;1.0;0.8;0.6;0.6
                        0.5;0.5;0.6;0.7;0.7;0.6;0.5;0.5
                        0.4;0.4;0.4;0.4;0.4;0.4;0.4;0.4
                        0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0|]

let coef_b_pawn = ref [|0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0
                        0.4;0.4;0.4;0.4;0.4;0.4;0.4;0.4
                        0.5;0.5;0.6;0.7;0.7;0.6;0.5;0.5
                        0.6;0.6;0.8;1.0;1.0;0.8;0.6;0.6
                        0.6;0.6;0.8;1.0;1.0;0.8;0.6;0.6
                        0.4;0.4;0.5;0.6;0.6;0.5;0.4;0.4
                        0.4;0.4;0.4;0.5;0.5;0.4;0.4;0.4
                        0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0|]


                    
                    
                    *)


