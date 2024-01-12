open Utils

(* Ajoute l'ensemble des coups du bitboard dans la liste des coups possibles *)
let rec add_moves_to_list p n bitboard acc =
  match bitboard with
  | 0L -> acc
  | _ ->
      add_moves_to_list p n
        (Bitboard.pop_lsb bitboard)
        (Board.Chessmove (p, n, Bitboard.get_lsb bitboard) :: acc)

let rec add_unmoves_to_list p n acc = function
  | 0L -> acc
  | b ->
      add_unmoves_to_list p n
        (Board.Chessmove (p, Bitboard.get_lsb b, n) :: acc)
        (Bitboard.pop_lsb b)

(* Joue le coup et renvoie la nouvelle position *)
let play_move (chessboard : Board.chessboard) (move : Board.chessmove) =
  match (move, chessboard.color) with
  | ShortCastling, White ->
      {
        (* Petit roque blanc *)
        chessboard with
        wking = Bitboard.set_nth 0L 6;
        wrooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 7) 5;
        color = Black;
        wcastle = false;
      }
  | ShortCastling, Black ->
      {
        (*Petit roque noir *)
        chessboard with
        bking = Bitboard.set_nth 0L 62;
        brooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 63) 61;
        color = White;
        bcastle = false;
      }
  | LongCastling, White ->
      {
        (* Grand roque blanc*)
        chessboard with
        wking = Bitboard.set_nth 0L 2;
        wrooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 0) 3;
        color = Black;
        wcastle = false;
      }
  | LongCastling, Black ->
      {
        (* Grand roque noir *)
        chessboard with
        bking = Bitboard.set_nth 0L 58;
        brooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.wrooks 56) 59;
        color = White;
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
          color = Board.change_color chessboard.color;
        }
      in
      (* On met la piece sur la case d'arrivee *)
      match (p, chessboard.color) with
      | P, White ->
          { tempboard with wpawns = Bitboard.set_nth tempboard.wpawns n_to }
      | B, White ->
          { tempboard with wbishops = Bitboard.set_nth tempboard.wbishops n_to }
      | N, White ->
          { tempboard with wknights = Bitboard.set_nth tempboard.wknights n_to }
      | R, White ->
          { tempboard with wrooks = Bitboard.set_nth tempboard.wrooks n_to }
      | Q, White ->
          { tempboard with wqueen = Bitboard.set_nth tempboard.wqueen n_to }
      | K, White ->
          {
            tempboard with
            wking = Bitboard.set_nth tempboard.wking n_to;
            wcastle = false;
          }
      | P, Black ->
          { tempboard with bpawns = Bitboard.set_nth tempboard.bpawns n_to }
      | B, Black ->
          { tempboard with bbishops = Bitboard.set_nth tempboard.bbishops n_to }
      | N, Black ->
          { tempboard with bknights = Bitboard.set_nth tempboard.bknights n_to }
      | R, Black ->
          { tempboard with brooks = Bitboard.set_nth tempboard.brooks n_to }
      | Q, Black ->
          { tempboard with bqueen = Bitboard.set_nth tempboard.bqueen n_to }
      | _ ->
          {
            tempboard with
            bking = Bitboard.set_nth tempboard.bking n_to;
            bcastle = false;
          })

let play_unmove chessboard = function
  | Board.Chessmove (p, n_from, n_to) ->
      Board.change_turn
        (play_move
           (Board.change_turn chessboard)
           (Board.Chessmove (p, n_to, n_from)))
  | _ -> failwith "Cas impossible"

(* Affiche la liste des coups possibles *)
let rec print_moves board moves =
  match moves with
  | [] -> ()
  | h :: t ->
      Board.print_board (play_move board h);
      print_moves board t
