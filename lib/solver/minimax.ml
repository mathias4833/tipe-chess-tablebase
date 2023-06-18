open Moves;;

(* Genere les coups l'ensemble des coups legaux *)
let generate_legal_moves board =
  let rec remove_illegal_moves moves acc =
    match moves with
    |[] -> acc
    (* Le coup est legal *)
    |h::t when Check.is_legal (Move.play_move board h) -> remove_illegal_moves t (h::acc)
    (* Le coup n'est pas legal, on ne l'ajoute pas a l'accumulateur *)
    |_::t -> remove_illegal_moves t acc
  in
  remove_illegal_moves (King.generate_moves board)
    (remove_illegal_moves (Queen.generate_moves board)
    (remove_illegal_moves (Rook.generate_moves board)
    (remove_illegal_moves (Bishop.generate_moves board)
    (remove_illegal_moves (Knight.generate_moves board)
    (remove_illegal_moves (Pawn.generate_moves board) 
    (remove_illegal_moves (Castling.generate_moves board) []))))))
;;

(* Algorithme minimax de recherche de mat *)
let minimax board depth =
  let rec minimax_aux board depth is_maximizing =
    (* Compare les deux noeux et garde le plus petit / plus grand suivant si on cherche le min ou le max *)
    let compare_nodes (v, b) m =
      let (v2, b2) = (minimax_aux (Move.play_move board m) (depth - 1) (not is_maximizing)) in
      if (is_maximizing && v > v2) || ((not is_maximizing) && v < v2) then
        (v, b)
      else
        (v2, b2)
    in
    
    if depth = 0 then
      (* Le joueur qui doit mater a mis en echecs l'adversaire et celui ci n'a plus de coup possible *)
      if (not is_maximizing) && (Check.is_check board) && ((generate_legal_moves board) = []) then
        (1, board)
      else
        (0, board)
    else if is_maximizing then (
      (* On recuper la valeur maxmimale des enfants du noeud*)
      List.fold_left compare_nodes (0, board) (generate_legal_moves board)
    ) else (
      (* On recuper la valeur minimale des enfants du noeud*)
      List.fold_left compare_nodes (1, board) (generate_legal_moves board)
    ) 
  in minimax_aux board depth true
;;
