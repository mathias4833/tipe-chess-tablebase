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

let _table = Endgame.Table_rook.generate_table 2 in
let _pos =
  {
    (Utils.Board.empty_board true) with
    wking = 0x40000000000L;
    bking = 0x10000000000L;
    wrooks = 0x400000000000000L;
  }
in
(* Printfwprintf "%b" (Hashtbl.mem _table _pos) *)
Hashtbl.iter
  (fun b (Endgame.Table_rook.Win (_, n)) ->
    if n = 2 then Utils.Board.print_board b)
  _table
(* List.iter *)
(* (fun b -> Utils.Board.print_board b) *)
(* (Endgame.Table_rook.generate_mates ()) *)
