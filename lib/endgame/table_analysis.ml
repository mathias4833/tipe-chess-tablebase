open Bigarray
open Utils
open Moves

(** [find_checkmate table pieces board] trouve la sequence de coups menant a un mat
    @param table Table de fin de partie
    @param pieces Liste des pièces présentes sur le plateau.
    @param board Plateau d'échecs initial.
    @return La liste des plateaux si le mat existe, [None] sinon. *)
let find_checkmate table pieces board =
  if not (Check.is_legal board) then failwith "La position n'est pas legale."
  else
    (* Fonction auxiliaire pour trouver le meilleur coup récursivement *)
    let rec find_best_move board acc_index acc_board = function
      | [] -> (acc_index, acc_board)
      | h :: t -> (
          let new_board =
            Transformations.normalize_board (Move.play_move board h) pieces
          in
          let new_index = Serializer.board_to_number new_board pieces in
          let is_white = Board.is_white board in
          let is_legal = Check.is_legal new_board in
          let is_lost = table.{new_index} = 0 in
          match acc_index with
          | _ when not is_legal -> find_best_move board acc_index acc_board t
          | None when is_white && is_lost ->
              find_best_move board acc_index acc_board t
          | Some i
            when let disadvantage_white =
                   table.{new_index} > table.{i} || is_lost
                 in
                 (is_white && disadvantage_white)
                 || ((not is_white) && not disadvantage_white) ->
              find_best_move board acc_index acc_board t
          | _ -> find_best_move board (Some new_index) new_board t)
    in

    (* Fonction principale pour trouver l'échec et mat *)
    let rec find_checkmate_aux acc board =
      let index = Serializer.board_to_number board pieces in
      if table.{index} = 0 then None
      else if table.{index} = 1 then Some (List.rev acc)
      else
        let moves = Move_generation.generate_legal_moves board in
        let best_index, best_board = find_best_move board None board moves in
        match best_index with
        | None -> failwith "La table est malformee, pas de coup suivant trouve."
        | Some i when table.{i} <> table.{index} - 1 ->
            failwith
              "La table est malformee, le coup suivant n'est pas le coup \
               attendu."
        | _ -> find_checkmate_aux (best_board :: acc) best_board
    in
    find_checkmate_aux [ board ] board

(** [check_integrity table pieces] verifie l'exactitude de la table de fin de partie.
    @param table Table de fin de partie
    @param pieces Liste des pièces présentes sur le plateau. *)
let check_integrity (table : _ Array1.t) pieces =
  let expected_size = 462 * (1 lsl (6 * List.length pieces)) * 2 in
  let size = Array1.dim table in
  if size <> expected_size then
    failwith
      (Printf.sprintf "La table contient %d, attendu %d" size expected_size)
  else
    for i = 0 to size - 1 do
      let board = Serializer.number_to_board i pieces in
      if Check.is_legal board then ignore (find_checkmate table pieces board)
    done
