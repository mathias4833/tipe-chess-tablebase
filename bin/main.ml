open Solver
open Utils

let measure_time board depth =
  let t = Sys.time () in
  let _, b = Minimax.minimax board depth in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  Board.print_board b
;;

measure_time Minimax.mat_in_1 1
