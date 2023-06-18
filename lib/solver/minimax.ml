open Moves;;
open Utils;;

(* Genere l'ensemble des coups legaux *)
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
      (* Calcul recursif de la valeur des noeuds enfants *)
      let (v2, b2) = (minimax_aux (Move.play_move board m) (depth - 1) (not is_maximizing)) in
      if (is_maximizing && v > v2) || ((not is_maximizing) && v < v2) then
        (v, b)
      else
        (v2, b2)
    in
    
    let moves = generate_legal_moves board in

    (* Si le noeud est sur une feuille de l'arbre *)
    if depth = 0 || (moves = []) then (
      (* Le joueur qui doit mater a mis en echecs l'adversaire et celui ci n'a plus de coup possible *)
      if (not is_maximizing) && (Check.is_check board) && (moves = []) then
        (1, board)
      else
        (0, board)
    ) else if is_maximizing then (
      (* On recupere la valeur maxmimale des enfants du noeud*)
      List.fold_left compare_nodes (0, board) moves
    ) else (
      (* On recupere la valeur minimale des enfants du noeud*)
      List.fold_left compare_nodes (1, board) moves
    ) 
  in minimax_aux board depth true
;;



(* Graphe *)

(* No. 27 *)
let (mat_in_1: Board.chessboard) = {
  wpawns = 0x1000L;
  wknights = 0x8000000L;
  wbishops = 0x20000000000000L;
  wrooks = 0x8L;
  wqueen = 0x1L;
  wking = 0x40L;
  bpawns = 0x102010000000L;
  bknights = 0L;
  bbishops = 0L;
  brooks = 0L;
  bqueen = 0L;
  bking = 0x1000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No. 37 *)
let (mat_in_1bis: Board.chessboard) = {
  wpawns = 0L;
  wknights = 0L;
  wbishops = 0L;
  wrooks = 0x4000000000000008L;
  wqueen = 0x8000000L;
  wking = 0x40L;
  bpawns = 0x2000000000000L;
  bknights = 0x4000000000000L;
  bbishops = 0x8000000000000L;
  brooks = 0xa00000000000000L;
  bqueen = 0L;
  bking = 0x400000000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No 159 *)
let (mat_in_1tres: Board.chessboard) = {
  wpawns = 0x100000L;
  wknights = 0x800L;
  wbishops = 0x1L;
  wrooks = 0L;
  wqueen = 0x40L;
  wking = 0x1000L;
  bpawns = 0x80000000000L;
  bknights = 0x8002000000000L;
  bbishops = 0x10000000000000L;
  brooks = 0L;
  bqueen = 0x2000000L;
  bking = 0x100000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No. 529 *)
let (mat_in_2: Board.chessboard) = {
  wpawns = 0x400000000000L;
  wknights = 0L;
  wbishops = 0x10000000L;
  wrooks = 0L;
  wqueen = 0x100000000000000L;
  wking = 0x10000000000000L;
  bpawns = 0x80800000000000L;
  bknights = 0L;
  bbishops = 0L;
  brooks = 0L;
  bqueen = 0x4000000000000000L;
  bking = 0x8000000000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No. 539 *)
let (mat_in_2bis: Board.chessboard) = {
  wpawns = 0xa0400L;
  wknights = 0x8000000L;
  wbishops = 0x100000000000L;
  wrooks = 0L;
  wqueen = 0x200L;
  wking = 0x400000000L;
  bpawns = 0x14042000008000L;
  bknights = 0L;
  bbishops = 0x2000000000000000L;
  brooks = 0x1000000000000080L;
  bqueen = 0x40L;
  bking = 0x800000000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No. 379 *)
let (mat_in_2tres: Board.chessboard) = {
  wpawns = 0L;
  wknights = 0x1040000L;
  wbishops = 0L;
  wrooks = 0x8L;
  wqueen = 0L;
  wking = 0x4000000L;
  bpawns = 0x900L;
  bknights = 0L;
  bbishops = 0L;
  brooks = 0L;
  bqueen = 0L;
  bking = 0x10000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No. 3295 *)
let (mat_in_3: Board.chessboard) = {
  wpawns = 0x100000L;
  wknights = 0L;
  wbishops = 0x10000000L;
  wrooks = 0x40L;
  wqueen = 0x800000000000L;
  wking = 0x80L;
  bpawns = 0x400000L;
  bknights = 0L;
  bbishops = 0L;
  brooks = 0L;
  bqueen = 0L;
  bking = 0x40000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

(* No. 4262 *)
let (mat_in_3bis: Board.chessboard) = {
  wpawns = 0x804000L;
  wknights = 0L;
  wbishops = 0L;
  wrooks = 0x201000000000000L;
  wqueen = 0x4000000000000L;
  wking = 0x8000L;
  bpawns = 0x20408800000000L;
  bknights = 0L;
  bbishops = 0L;
  brooks = 0x100000080000L;
  bqueen = 0x100000L;
  bking = 0x40000000000000L;
  iswhite = false;
  wcastle = false;
  bcastle = false
};;

let (mat_in_4: Board.chessboard) = {
  wpawns = 0x800000000000L;
  wknights = 0x200000020000L;
  wbishops = 0L;
  wrooks = 0L;
  wqueen = 0L;
  wking = 0x10L;
  bpawns = 0x1000001000L;
  bknights = 0L;
  bbishops = 0L;
  brooks = 0L;
  bqueen = 0L;
  bking = 0x8000000000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;

let (mat_in_4bis: Board.chessboard) = {
  wpawns = 0x251a000L;
  wknights = 0L;
  wbishops = 0x200000000000L;
  wrooks = 0x8000000000000L;
  wqueen = 0x8000000000L;
  wking = 0x40L;
  bpawns = 0x4810204000000L;
  bknights = 0x10000000000000L;
  bbishops = 0x20000000000L;
  brooks = 0x3000000000000000L;
  bqueen = 0L;
  bking = 0x4000000000000000L;
  iswhite = true;
  wcastle = false;
  bcastle = false
};;