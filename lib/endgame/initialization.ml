(*
open Moves
open Utils

(* Genere l'ensemble des positions avec le nombre exact de pieces*)
let generate_all_pos (p : Board.chesspieces) =
  (* Renvoie l'ensemble des positions possibles en ajoutant une piece précise *)
  let rec add_piece board piece acc = function
    | i when i > 63 -> List.map (fun b -> Board.update_board board b piece) acc
    | i when Bitboard.get_nth (Board.piece_to_bitboard board piece) i = 1L ->
        add_piece board piece acc (i + 1)
    | i ->
        add_piece board piece
          (Bitboard.set_nth (Board.piece_to_bitboard board piece) i :: acc)
          (i + 1)
  in
  let rec aux acc = function
    | [] -> acc
    | piece :: t ->
        aux (List.concat_map (fun board -> add_piece board piece [] 0) acc) t
  in
  (* Position vide au noir de jouer car on cherche un mat *)
  aux [ { Board.empty_board with iswhite = false } ] p

(* Ne conserve que les positions qui sont actuellement des mats *)
let generate_all_mates (p : Board.chesspieces) =
  List.filter (fun board -> Check.is_check board) (generate_all_pos p)
*)
