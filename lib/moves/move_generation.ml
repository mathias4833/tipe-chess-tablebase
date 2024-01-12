open Utils

(* Genere l'ensemble des coups legaux *)
let generate_legal_moves board =
  let rec remove_illegal_moves acc = function
    | [] -> acc
    (* Le coup est legal *)
    | h :: t when Check.is_legal (Move.play_move board h) ->
        remove_illegal_moves (h :: acc) t
    (* Le coup n'est pas legal, on ne l'ajoute pas a l'accumulateur *)
    | _ :: t -> remove_illegal_moves acc t
  in
  List.fold_left remove_illegal_moves []
    [
      King.generate_moves board;
      Queen.generate_moves board;
      Rook.generate_moves board;
      Bishop.generate_moves board;
      Knight.generate_moves board;
      Pawn.generate_moves board;
      Castling.generate_moves board;
    ]

(* Genere l'ensemble des coups legaux qui auraient pu etre joué avant *)
let generate_legal_unmoves board =
  let rec remove_illegal_unmoves acc = function
    | [] -> acc
    (* Le coup est legal *)
    | h :: t
      when let b = Move.play_unmove board h in
           Check.is_legal b ->
        remove_illegal_unmoves (h :: acc) t
    (* Le coup n'est pas legal, on ne l'ajoute pas a l'accumulateur *)
    | _ :: t -> remove_illegal_unmoves acc t
  in
  (* On change le tour pour obtenir les coups des pieces adverses *)
  let b = Board.change_turn board in
  List.fold_left remove_illegal_unmoves []
    [
      King.generate_unmoves b;
      Queen.generate_unmoves b;
      Rook.generate_unmoves b;
      Bishop.generate_unmoves b;
      Knight.generate_unmoves b;
    ]
