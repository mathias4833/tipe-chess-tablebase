(*
open Solver
open Utils


let measure_time board depth =
  let t = Sys.time () in
  let _, b = Minimax.minimax board depth in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  Board.print_board b
;;

measure_time Minimax.mat_in_1 1
*)

let _n = 33 in
let _table = Endgame.Table_rook.generate_table _n in

(* Printf.printf "%b" (Hashtbl.mem _table _pos); *)
Hashtbl.iter
  (fun b e ->
    match e with
    | Endgame.Table_rook.Win (Utils.Board.Chessmove (_, f, t), n) ->
        if n = _n then (
          Utils.Board.print_board b;
          Printf.printf "%i %i\n" f t)
    | _ -> ())
  _table;
Printf.printf "%i" (Hashtbl.length _table)

(* List.iter *)
(* (fun b -> Utils.Board.print_board b) *)
(* (Endgame.Table_rook.generate_mates ()) *)
