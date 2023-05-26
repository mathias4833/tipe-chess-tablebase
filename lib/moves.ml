(*let generate_moves chessboard =
  let moves = [] in
  let moves2 = generate_pawns_moves chessboard in
  let moves3 = generate_rook_moves chessboard in
  let moves4 = generate_bishop_moves chessboard in
  let moves4 = generate_king_moves chessboard in
;;*)