open Utils
open Moves

(* Genere l'ensemble des positions de mats avec deux rois et une tour *)
let generate_mates pieces =
  let rec generate_all_positions acc = function
    | [] ->
        List.filter
          (fun b ->
            Check.is_check b && Check.is_legal b
            && Move_generation.generate_legal_moves b = [])
          acc
    | h :: t ->
        generate_all_positions
          (List.concat_map
             (fun b ->
               let positions =
                 List.filter
                   (fun bo -> Transformations.is_normalized bo pieces)
                   (Board.add_piece b h)
               in
               if fst h = K then positions else b :: positions)
             acc)
          t
  in
  generate_all_positions
    [ Board.empty_board Board.Black ]
    ((Board.K, Board.White) :: (Board.K, Board.Black) :: pieces)

(* Ajoute la position si elle n'est pas deja présente *)
let add_unmove table pieces (board : Board.chessboard) (m : Board.chessmove) i =
  let board_prev = Move.play_unmove board m in
  let norm_board = Transformations.normalize_board board_prev pieces in
  let number = Serializer.board_to_number norm_board pieces in

  if table.{number} = 0 then
    if i mod 2 = 0 then (
      (* Au blanc de jouer, on ajoute le coup *)
      table.{number} <- i;
      Some norm_board)
    else if
      (* Au noirs de jouer, on verifie que tous les coups possibles perdent *)
      List.for_all
        (fun next_move ->
          let board_next = Move.play_move norm_board next_move in
          let board_next_norm =
            Transformations.normalize_board board_next pieces
          in
          let board_number =
            Serializer.board_to_number board_next_norm pieces
          in
          table.{board_number} <> 0)
        (Move_generation.generate_legal_moves norm_board)
    then (
      table.{number} <- i;
      Some norm_board)
    else None
  else None

(* Genere les tables pour le jeu de pieces jusqu'au mat en n demi-coups *)
let generate_endgames pieces n =
  Printf.printf "Creation du tableau\n%!";
  let file_descr, table = Serializer.open_table pieces in

  let rec aux acc = function
    | i when i > n ->
        Printf.printf "--> %i\n%!" (List.length acc);
        Serializer.close_table file_descr;
        table
    | i ->
        (* Genere l'ensemble des coups precedents et l'ajoute a la table *)
        Printf.printf "Profondeur %i; %i pos\n%!" (i - 1) (List.length acc);
        aux
          (List.concat_map
             (fun b ->
               List.filter_map
                 (fun m -> add_unmove table pieces b m i)
                 (Move_generation.generate_legal_unmoves b pieces))
             acc)
          (i + 1)
  in
  Printf.printf "Generation des mats\n%!";
  let mates = generate_mates pieces in
  Printf.printf "Profondeur 1; %i\n%!" (List.length mates);
  List.iter (fun b -> table.{Serializer.board_to_number b pieces} <- 1) mates;
  aux mates 2
