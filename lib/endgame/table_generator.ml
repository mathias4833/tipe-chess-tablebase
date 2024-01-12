open Utils
open Moves

type evaluation = AlreadyWon | Win of Board.chessmove * int

(* Genere l'ensemble des positions de mats avec deux rois et une tour *)
let generate_mates pieces =
  let rec generate_all_positions acc = function
    | [] ->
        List.filter
          (fun b ->
            Transformations.is_normalized b pieces
            && Check.is_check b && Check.is_legal b
            && Move_generation.generate_legal_moves b = [])
          acc
    | h :: t ->
        generate_all_positions
          (List.concat_map (fun b -> Board.add_piece b h) acc)
          t
  in
  generate_all_positions [ Board.empty_board Board.Black ] pieces

(* Ajoute la position si elle n'est pas deja présente *)
let add_unmove table pieces (b : Board.chessboard) (m : Board.chessmove) i =
  let board_prev = Move.play_unmove b m in
  let board_norm, move_norm =
    Transformations.normalize_board board_prev m pieces
  in
  let key = Board.board_to_number board_norm pieces in

  if not (Hashtbl.mem table key) then
    if i mod 2 = 1 then (
      (* Au blanc de jouer, on ajoute le coup *)
      Hashtbl.add table key (Win (move_norm, i));
      Some board_norm)
    else if
      (* Au noirs de jouer, on verifie que tous les coups possibles perdent *)
      List.for_all
        (fun move_next ->
          let board_next = Move.play_move board_norm move_next in
          let board_next_norm, _ =
            Transformations.normalize_board board_next move_next pieces
          in
          Hashtbl.mem table (Board.board_to_number board_next_norm pieces))
        (Move_generation.generate_legal_moves board_norm)
    then (
      Hashtbl.add table key (Win (move_norm, i));
      Some board_norm)
    else None
  else None

(* Genere la table pour le jeu de pieces jusqu'au mat en n demi-coups *)
let generate_table pieces n =
  let table = Hashtbl.create 65536 in
  let rec aux acc = function
    | i when i > n -> table
    | i ->
        (* Genere l'ensemble des coups precedents et l'ajoute a la table *)
        Printf.printf "Generation de la profondeur %i. %i positions.\n%!" i
          (Hashtbl.length table);
        aux
          (List.concat_map
             (fun b ->
               List.filter_map
                 (fun m -> add_unmove table pieces b m i)
                 (Move_generation.generate_legal_unmoves b))
             acc)
          (i + 1)
  in
  let mates = generate_mates pieces in
  List.iter
    (fun b -> Hashtbl.add table (Board.board_to_number b pieces) AlreadyWon)
    mates;
  aux mates 1
