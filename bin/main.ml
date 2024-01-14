open Utils
open Endgame;;

let _nb_coups = 1 in
(* let _n = (_nb_coups * 2) - 1 in *)
let _n = 65 in
let _pieces =
  (* [ (Board.K, Board.White); (Board.K, Board.Black); (Board.R, Board.White) ] *)
  [
    (Board.K, Board.White);
    (Board.K, Board.Black);
    (Board.N, Board.White);
    (Board.B, Board.White);
  ]
in
let _table = Table_generator.generate_table _pieces _n in
let _count = ref 0 in

Hashtbl.iter
  (fun b e ->
    match e with
    | Table_generator.Win (Board.Chessmove (_, f, t), n) ->
        if n = _n then (
          let i1, j1 = Bitboard.coord_of_index f in
          let i2, j2 = Bitboard.coord_of_index t in
          let _board = Board.number_to_board b _pieces in
          incr _count;
          Board.print_board _board;
          Printf.printf "Mat en %i demi-coups: (%i, %i) --> (%i, %i)\n\n%!" _n
            i1 j1 i2 j2)
    | _ -> ())
  _table;

Printf.printf "La table contient %i positions\n%!" (Hashtbl.length _table);
Printf.printf "Nombre de mats en %i demi-coups: %i \n%!" _n !_count

(* List.iter *)
(*   (fun b -> Utils.Board.print_board b) *)
(*   (Endgame.Table_generator.generate_mates _pieces) *)
