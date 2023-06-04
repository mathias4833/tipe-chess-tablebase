open Int64;;
open Utils;;

(* Genere l'ensemble des coups pour la dame *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let whole = Board.get_whole_board chessboard in
  
  let rec generate_moves_aux board acc =
    match board with
    |0L -> acc
    |_ -> (
      let n = Bitboard.get_lsb board in
      (* Bitboard contenant l'ensemble des pieces bloquantes pour le fou*)
      let blockerboard_bishop = logand whole (Bishop.table_mask.(n)) in
      let all_moves_bishop = logand (Hashtbl.find Bishop.table_moves.(n) blockerboard_bishop) (lognot ally) in

      (* Bitboard contenant l'ensemble des pieces bloquantes pour la tour *)
      let blockerboard_rook = logand whole (Rook.table_mask.(n)) in
      let all_moves_rook = logand (Hashtbl.find Rook.table_moves.(n) blockerboard_rook) (lognot ally) in

      let all_moves = logor all_moves_bishop all_moves_rook in
      generate_moves_aux (Bitboard.pop_lsb board) (Bitboard.add_moves_to_list all_moves acc)
    )
  in
  generate_moves_aux (Board.if_w_else chessboard chessboard.wbishops chessboard.bbishops) []
;;
