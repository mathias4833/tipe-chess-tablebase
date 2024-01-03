open Utils
open Moves

(* Genere l'ensemble des coups legaux *)
let generate_legal_moves board =
  let rec remove_illegal_moves moves acc =
    match moves with
    | [] -> acc
    (* Le coup est legal *)
    | h :: t when Check.is_legal (Move.play_move board h) ->
        remove_illegal_moves t (h :: acc)
    (* Le coup n'est pas legal, on ne l'ajoute pas a l'accumulateur *)
    | _ :: t -> remove_illegal_moves t acc
  in
  remove_illegal_moves
    (King.generate_moves board)
    (remove_illegal_moves
       (Queen.generate_moves board)
       (remove_illegal_moves
          (Rook.generate_moves board)
          (remove_illegal_moves
             (Bishop.generate_moves board)
             (remove_illegal_moves
                (Knight.generate_moves board)
                (remove_illegal_moves
                   (Pawn.generate_moves board)
                   (remove_illegal_moves (Castling.generate_moves board) []))))))

(* Genere l'ensemble des coups legaux qui auraient pu etre joué avant *)
let generate_previous_legal_moves (board : Board.chessboard) =
  let b = { board with iswhite = not board.iswhite } in
  let rec remove_illegal_moves moves acc =
    match moves with
    | [] -> acc
    (* Le coup est legal *)
    | h :: t when not (Check.is_check (Move.play_move b h)) ->
        remove_illegal_moves t (h :: acc)
    (* Le coup n'est pas legal, on ne l'ajoute pas a l'accumulateur *)
    | _ :: t -> remove_illegal_moves t acc
  in
  remove_illegal_moves (King.generate_moves b)
    (remove_illegal_moves (Queen.generate_moves b)
       (remove_illegal_moves (Rook.generate_moves b)
          (remove_illegal_moves (Bishop.generate_moves b)
             (remove_illegal_moves (Knight.generate_moves b)
                (remove_illegal_moves (Pawn.generate_moves b) [])))))
