(*

open Int64;;

(* Renvoie la position des cases accessible par prise *)
let capture_pawn is_white coord blockers_board =
  if is_white then
    logand Tables.w_pawns_attacks.(coord) blockers_board
  else
    logand Tables.b_pawns_attacks.(coord) blockers_board
;;

(* Renvoie la position des cases accessible en avancant *)
let move_pawn is_white (i, j) blockers_board =
  let move =
    if is_white then 
      Utils.create_board (i+1) j
    else
      Utils.create_board (i-1) j
  in logand move (lognot blockers_board)
;;

(* Genere l'ensemble des coups pour les pions *)
let generate_moves chessboard =
  let enemy_board = Utils.enemy_board chessboard in
  let friendly_board = Utils.friendly_board chessboard in
  
  let rec generate_moves_aux p_board acc =
    let move = Utils.isolate_lsb p_board in
    
  in generate_moves_aux chessboard.w_pawns []
  
;;
*)
