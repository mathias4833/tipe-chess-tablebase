open Utils

let update_castling_rights_after_rook_move (board : Board.chessboard) color piece n_from =
  match (piece, color, n_from) with
  | Board.R, Board.White, 0 | Board.R, Board.White, 7 ->
      { board with wcastle = false }
  | Board.R, Board.Black, 56 | Board.R, Board.Black, 63 ->
      { board with bcastle = false }
  | _ -> board

let update_castling_rights_after_capture (board : Board.chessboard) captured_piece n_to =
  match (captured_piece, n_to) with
  | Some (Board.R, Board.White), (0 | 7) -> { board with wcastle = false }
  | Some (Board.R, Board.Black), (56 | 63) -> { board with bcastle = false }
  | _ -> board

(** [add_moves_to_list p n bitboard acc] ajoute l'ensemble des coups possibles dans la liste des coups.
    @param p La pièce concernée par le coup.
    @param n L'indice de la position de la pièce sur l'échiquier.
    @param bitboard Le bitboard représentant les positions possibles de la pièce.
    @param acc La liste de coups actuelle.
    @return La liste de coups mise à jour avec les nouveaux coups possibles. *)
let rec add_moves_to_list p n bitboard acc =
  match bitboard with
  | 0L -> acc
  | _ ->
      add_moves_to_list p n
        (Bitboard.pop_lsb bitboard)
        (Board.Chessmove (p, n, Bitboard.get_lsb bitboard, None) :: acc)

(** [add_unmoves_to_list p n acc board pieces bitboard] ajoute l'ensemble des annulations de coups possibles dans la liste des coups.
    @param p La pièce concernée par le coup.
    @param n L'indice de la position d'arrivée du coup sur l'échiquier.
    @param acc La liste d'annulations de coups actuelle.
    @param board Le plateau d'échecs.
    @param pieces La liste des pièces sur l'échiquier.
    @param bitboard Le bitboard représentant les positions possibles de départ du coup.
    @return La liste mise à jour avec les nouvelles annulations de coups possibles. *)
let rec add_unmoves_to_list p n acc board pieces = function
  | 0L -> acc
  | b ->
      let from = Bitboard.get_lsb b in
      let moves =
        List.filter_map
          (fun x ->
            if Board.get_bitboard board x = 0L then
              Some (Board.Chessmove (p, from, n, Some x))
            else None)
          pieces
      in
      add_unmoves_to_list p n
        (Board.Chessmove (p, from, n, None) :: (moves @ acc))
        board pieces (Bitboard.pop_lsb b)

(** [play_move chessboard move] execute d'un coup sur le plateau d'échecs.
    @param chessboard Le plateau d'échecs actuel.
    @param move Le coup à jouer.
    @return Le nouveau plateau d'échecs après l'exécution du coup. *)
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
        brooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.brooks 63) 61;
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
        brooks = Bitboard.set_nth (Bitboard.clear_nth chessboard.brooks 56) 59;
        color = White;
        bcastle = false;
      }
  | Chessmove (p, n_from, n_to, oq), _ -> (
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
        |> fun board ->
        update_castling_rights_after_rook_move board chessboard.color p n_from
        |> fun board -> update_castling_rights_after_capture board oq n_to
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

(** [play_unmove chessboard coup] simule l'annulation d'un coup sur le plateau d'échecs.
    @param chessboard Le plateau d'échecs actuel.
    @param coup Le coup à annuler.
    @return Le nouveau plateau d'échecs après l'annulation du coup. *)
let play_unmove chessboard = function
  | Board.Chessmove (p, n_from, n_to, oq) -> (
      let board =
        Board.change_turn
          (play_move
             (Board.change_turn chessboard)
             (Board.Chessmove (p, n_to, n_from, oq)))
      in
      match oq with
      | None -> board
      | Some q ->
          let restored =
            Bitboard.set_nth (Board.get_bitboard board q) n_to
          in
          Board.set_bitboard board restored q)
  | _ -> failwith "Cas impossible"

(** [print_moves board moves] affiche les plateaux d'échecs après l'exécution de chaque coup.
    @param board Le plateau d'échecs initial.
    @param moves La liste des coups à jouer. *)
let rec print_moves board moves =
  match moves with
  | [] -> ()
  | h :: t ->
      Board.print_board (play_move board h);
      print_moves board t
