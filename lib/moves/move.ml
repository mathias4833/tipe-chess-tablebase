open Utils;;

type piece = P | B | N | R | Q | K;;
(* Piece qui bouge / case de depart / case d'arrivee / piece mangee *)
type chessmove = Move of piece * int * int;;


(* Ajoute l'ensemble des coups du bitboard dans la liste des coups possibles *)
let rec add_moves_to_list p n bitboard acc = 
  match bitboard with
  |0L -> acc
  |_ -> add_moves_to_list p n (Bitboard.pop_lsb bitboard) (Move(p, n, Bitboard.get_lsb bitboard)::acc)
;;

(* Joue le coup et renvoie la nouvelle position *)
let play_move chessboard move =
  let tempboard = {
    chessboard with
    wpawns = Bitboard.clear_nth chessboard.wpawns a;
    wknights = Bitboard.clear_nth chessboard.wknights a;
    wbishops = Bitboard.clear_nth chessboard.wbishops a;
    wrooks = Bitboard.clear_nth chessboard.wrooks a;
    wqueen = Bitboard.clear_nth chessboard.wqueen a;
    wking = Bitboard.clear_nth chessboard.wking a;
    bpawns = Bitboard.clear_nth chessboard.bpawns a;
    bknights = Bitboard.clear_nth chessboard.bknights a;
    bbishops = Bitboard.clear_nth chessboard.bbishops a;
    brooks = Bitboard.clear_nth chessboard.brooks a;
    bqueen = Bitboard.clear_nth chessboard.bqueen a;
    bking = Bitboard.clear_nth chessboard.bking a;
    iswhite: if chessboard.iswhite then false else true;
  } in

  match (move, chessboard.iswhite) with
  |(Move(P, a, b), true) -> {
      
    }
;;

(* Affiche l'ensemble des coups possibles *)
let rec print_list_moves chessboard moves =
  match moves with
  |[] -> ()
  |m::t -> (
    
  )
;;
