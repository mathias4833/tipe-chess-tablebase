open Utils
open Moves

type evaluation = AlreadyWon | Win of Board.chessmove * int

(* Genere l'ensemble des positions de mats avec deux rois et une tour *)
let generate_mates () =
  (* On ne garde que les positions où le Roi se trouve dans le triangle du bas *)
  let boards =
    List.filter
      (fun b -> Transformations.is_normalized b)
      (Board.add_piece Board.empty_board Board.K)
  in
  let boards = List.concat_map (fun b -> Board.add_piece b Board.R) boards in
  let boards =
    List.concat_map
      (fun b -> Board.add_piece (Board.change_turn b) Board.K)
      boards
  in
  List.filter
    (fun b ->
      Check.is_check b && Check.is_legal b
      && Move_generation.generate_legal_moves b = [])
    boards

let generate_table n =
  let table = Hashtbl.create 65536 in
  (* Ajoute la position si elle n'est pas deja présente *)
  let add_unmove (b : Board.chessboard) m i =
    let prev_b = Move.play_unmove b m in

    if not (Hashtbl.mem table prev_b) then
      if i mod 2 = 1 then
        (* Au blancs de jouer, on verifie que le roi blanc est dans le triangle *)
        let n_board, n_move = Transformations.normalize_board prev_b m in
        if not (Hashtbl.mem table n_board) then (
          Hashtbl.add table n_board (Win (n_move, i));
          Some n_board)
        else None
      else if
        (* Au noirs de jouer, on verifie que tous les coups possibles perdent *)
        List.for_all
          (fun m -> Hashtbl.mem table (Move.play_move prev_b m))
          (Move_generation.generate_legal_moves prev_b)
      then (
        Hashtbl.add table prev_b (Win (m, i));
        Some prev_b)
      else None
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
                 (fun m -> add_unmove b m i)
                 (Move_generation.generate_legal_unmoves b))
             acc)
          (i + 1)
  in
  let mates = generate_mates () in
  List.iter (fun b -> Hashtbl.add table b AlreadyWon) mates;
  aux mates 1
