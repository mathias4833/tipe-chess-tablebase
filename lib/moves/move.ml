open Utils

(* Notation internationale des pieces *)
type piece = P | B | N | R | Q | K

(* Piece qui bouge / case de depart / case d'arrivee *)
type chessmove = Chessmove of piece * int * int | ShortCastling | LongCastling

(* Ajoute l'ensemble des coups du bitboard dans la liste des coups possibles *)
let rec add_moves_to_list p n bitboard acc =
  match bitboard with
  | 0L -> acc
  | _ ->
      add_moves_to_list p n
        (Bitboard.pop_lsb bitboard)
        (Chessmove (p, n, Bitboard.get_lsb bitboard) :: acc)

(* Joue le coup et renvoie la nouvelle position *)
let play_move (chessboard : Board.chessboard) move =
  match (move, chessboard.iswhite) with
  | ShortCastling, true ->
      {
        (* Petit roque blanc *)
        chessboard with
        wking = Bitboard.set_nth 0L 6;
        wrooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 7) 5;
        iswhite = false;
        wcastle = false;
      }
  | ShortCastling, false ->
      {
        (*Petit roque noir *)
        chessboard with
        bking = Bitboard.set_nth 0L 62;
        brooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 63) 61;
        iswhite = true;
        bcastle = false;
      }
  | LongCastling, true ->
      {
        (* Grand roque blanc*)
        chessboard with
        wking = Bitboard.set_nth 0L 2;
        wrooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 0) 3;
        iswhite = false;
        wcastle = false;
      }
  | LongCastling, false ->
      {
        (* Grand roque noir *)
        chessboard with
        bking = Bitboard.set_nth 0L 58;
        brooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 56) 59;
        iswhite = true;
        bcastle = false;
      }
  | Chessmove (p, n_from, n_to), _ -> (
      (* Echiquier temporaire sans aucune piece sur la case d'arrivee et de depart *)
      let clear b = Bitboard.clear_nth (Bitboard.clear_nth b n_from) n_to in
      let tempboard =
        {
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
          iswhite = not chessboard.iswhite;
        }
      in
      (* On met la piece sur la case d'arrivee *)
      match (p, chessboard.iswhite) with
      | P, true ->
          { tempboard with wpawns = Bitboard.set_nth tempboard.wpawns n_to }
      | B, true ->
          { tempboard with wbishops = Bitboard.set_nth tempboard.wbishops n_to }
      | N, true ->
          { tempboard with wknights = Bitboard.set_nth tempboard.wknights n_to }
      | R, true ->
          { tempboard with wrooks = Bitboard.set_nth tempboard.wrooks n_to }
      | Q, true ->
          { tempboard with wqueen = Bitboard.set_nth tempboard.wqueen n_to }
      | K, true ->
          {
            tempboard with
            wking = Bitboard.set_nth tempboard.wking n_to;
            wcastle = false;
          }
      | P, false ->
          { tempboard with bpawns = Bitboard.set_nth tempboard.bpawns n_to }
      | B, false ->
          { tempboard with bbishops = Bitboard.set_nth tempboard.bbishops n_to }
      | N, false ->
          { tempboard with bknights = Bitboard.set_nth tempboard.bknights n_to }
      | R, false ->
          { tempboard with brooks = Bitboard.set_nth tempboard.brooks n_to }
      | Q, false ->
          { tempboard with bqueen = Bitboard.set_nth tempboard.bqueen n_to }
      | _ ->
          {
            tempboard with
            bking = Bitboard.set_nth tempboard.bking n_to;
            bcastle = false;
          })

(* Affiche la liste des coups possibles *)
let rec print_moves board moves =
  match moves with
  | [] -> ()
  | h :: t ->
      Board.print_board (play_move board h);
      print_moves board t
