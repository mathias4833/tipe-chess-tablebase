open Int64
open Utils

(* Genere l'ensemble des coups pour la dame *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let whole = Board.get_whole_board chessboard in

  let rec generate_moves_aux board acc =
    match board with
    | 0L -> acc
    | _ ->
        (* Indice de la dame *)
        let n = Bitboard.get_lsb board in
        let moves =
          logor
            (Rook.moves_from_board ally whole n)
            (Bishop.moves_from_board ally whole n)
        in
        generate_moves_aux (Bitboard.pop_lsb board)
          (Move.add_moves_to_list Q n moves acc)
  in
  generate_moves_aux (Board.get_ally_bitboard chessboard Q) []

let generate_unmoves chessboard =
  let whole = Board.get_whole_board chessboard in
  let rec generate_unmoves_aux acc = function
    | 0L -> acc
    | b ->
        let n = Bitboard.get_lsb b in
        let moves =
          logor
            (Rook.unmoves_from_board whole n)
            (Bishop.unmoves_from_board whole n)
        in
        generate_unmoves_aux
          (Move.add_unmoves_to_list Q n acc moves)
          (Bitboard.pop_lsb b)
  in
  generate_unmoves_aux [] (Board.get_ally_bitboard chessboard Q)
