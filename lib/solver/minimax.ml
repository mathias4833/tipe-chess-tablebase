open Moves

(** [minimax board depth] applique l'algorithme minimax pour trouver la suite de coup gagnante.
    @param board Le plateau d'échecs actuel.
    @param depth La profondeur de recherche pour l'algorithme.
    @return Tuple (valeur de l'evalution, plateau resultant de la meilleure decision trouvée) *)
let minimax board depth =
  let rec minimax_aux board depth is_maximizing =
    (* Compare les deux noeux et garde le plus petit / plus grand suivant si on cherche le min ou le max *)
    let compare_nodes (v, b) m =
      (* Calcul recursif de la valeur des noeuds enfants *)
      let v2, b2 =
        minimax_aux (Move.play_move board m) (depth - 1) (not is_maximizing)
      in
      if (is_maximizing && v > v2) || ((not is_maximizing) && v < v2) then (v, b)
      else (v2, b2)
    in

    let moves = Move_generation.generate_legal_moves board in

    (* Si le noeud est sur une feuille de l'arbre *)
    if depth = 0 || moves = [] then
      if
        (* Le joueur qui doit mater a mis en echecs l'adversaire et celui ci n'a plus de coup possible *)
        (not is_maximizing) && Check.is_check board && moves = []
      then (1, board)
      else (0, board)
    else if is_maximizing then
      (* On recupere la valeur maxmimale des enfants du noeud*)
      List.fold_left compare_nodes (0, board) moves
    else
      (* On recupere la valeur minimale des enfants du noeud*)
      List.fold_left compare_nodes (1, board) moves
  in
  minimax_aux board depth true
