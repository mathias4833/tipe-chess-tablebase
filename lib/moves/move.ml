open Utils;;

type piece = P | B | N | R | Q | K;;
(* Piece qui bouge / case de depart / case d'arrivee *)
type chessmove = Chessmove of piece * int * int;;


(* Ajoute l'ensemble des coups du bitboard dans la liste des coups possibles *)
let rec add_moves_to_list p n bitboard acc = 
  match bitboard with
  |0L -> acc
  |_ -> add_moves_to_list p n (Bitboard.pop_lsb bitboard) (Chessmove(p, n, Bitboard.get_lsb bitboard)::acc)
;;

(* Joue le coup et renvoie la nouvelle position *)
let play_move (chessboard: Board.chessboard) move =
  let Chessmove(p, n_from, n_to) = move in
  (* Echiquier temporaire avec aucune piece sur la case d'arrivee et de depart *)
  let clear b = Bitboard.clear_nth (Bitboard.clear_nth b n_from) n_to in 
  let tempboard = {
    chessboard with
    wpawns = clear chessboard.wpawns;
    wknights = clear chessboard.wknights;
    wbishops = clear chessboard.wbishops;
    wrooks = clear chessboard.wrooks;
    wqueen = clear chessboard.wqueen;
    wking = clear chessboard.wking;
    bpawns = clear chessboard.bpawns;
    bknights = clear chessboard.bknights;
    bbishops = clear chessboard.bbishops;
    brooks = clear chessboard.brooks;
    bqueen = clear chessboard.bqueen;
    bking = clear chessboard.bking;
    iswhite = not chessboard.iswhite
  } in
  (* On enleve la piece de la case de depart et on la met sur la case d'arrivee *)
  match (p, chessboard.iswhite) with
  |(P, true) -> {tempboard with wpawns = Bitboard.set_nth (Bitboard.clear_nth tempboard.wpawns n_from) n_to}
  |(B, true) -> {tempboard with wbishops = Bitboard.set_nth (Bitboard.clear_nth tempboard.wbishops n_from) n_to}
  |(N, true) -> {tempboard with wknights = Bitboard.set_nth (Bitboard.clear_nth tempboard.wknights n_from) n_to}
  |(R, true) -> {tempboard with wrooks = Bitboard.set_nth (Bitboard.clear_nth tempboard.wrooks n_from) n_to}
  |(Q, true) -> {tempboard with wqueen = Bitboard.set_nth (Bitboard.clear_nth tempboard.wqueen n_from) n_to}
  |(K, true) -> {tempboard with wking = Bitboard.set_nth (Bitboard.clear_nth tempboard.wking n_from) n_to}
  |(P, false) -> {tempboard with bpawns = Bitboard.set_nth (Bitboard.clear_nth tempboard.bpawns n_from) n_to}
  |(B, false) -> {tempboard with bbishops = Bitboard.set_nth (Bitboard.clear_nth tempboard.bbishops n_from) n_to}
  |(N, false) -> {tempboard with bknights = Bitboard.set_nth (Bitboard.clear_nth tempboard.bknights n_from) n_to}
  |(R, false) -> {tempboard with brooks = Bitboard.set_nth (Bitboard.clear_nth tempboard.brooks n_from) n_to}
  |(Q, false) -> {tempboard with bqueen = Bitboard.set_nth (Bitboard.clear_nth tempboard.bqueen n_from) n_to}
  |_ -> {tempboard with bking = Bitboard.set_nth (Bitboard.clear_nth tempboard.bking n_from) n_to}
;;

(* Affiche la liste des coups possibles *)
let rec print_moves board moves =
  match moves with
  |[] -> ()
  |h::t -> (
    Board.print_board (play_move board h);
    print_moves board t
  )
;;

