open Utils
open Moves

type evaluation = Win of Move.chessmove * int

(* Genere l'ensemble des positions de mats avec deux rois et une tour *)
let generate_mates () =
  let boards = Board.add_piece (Board.empty_board false) Board.WKing in
  let boards2 =
    List.concat_map (fun b -> Board.add_piece b Board.BKing) boards
  in
  let boards3 =
    List.concat_map (fun b -> Board.add_piece b Board.WRrook) boards2
  in
  List.filter
    (fun b ->
      Check.is_check b && Check.is_legal b && Unmove.generate_legal_moves b = [])
    boards3

let generate_table n =
  let table = Hashtbl.create 65536 in
  (* Ajoute la position si elle n'est pas deja présente *)
  let add_move_to_table (b : Board.chessboard) m i =
    let prev_b = Move.play_move { b with iswhite = not b.iswhite } m in
    if not (Hashtbl.mem table prev_b) then (
      Hashtbl.add table prev_b (Win (m, i));
      Some prev_b)
    else None
  in
  let rec aux acc = function
    | i when i > n -> table
    | i ->
        (* Genere l'ensemble des coups precedents et l'ajoute a la table *)
        aux
          (List.concat_map
             (fun b ->
               List.filter_map
                 (fun m -> add_move_to_table b m i)
                 (Unmove.generate_previous_legal_moves b))
             acc)
          (i + 1)
  in
  aux (generate_mates ()) 1
