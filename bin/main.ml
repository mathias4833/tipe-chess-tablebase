open Utils
open Endgame;;

let _nb_coups = 1 in
(* let _n = (_nb_coups * 2) - 1 in *)
let _n = 65 in
let _pieces =
  (* [ (Board.K, Board.White); (Board.K, Board.Black); (Board.R, Board.White) ] *)
  [ (Board.N, Board.White); (Board.B, Board.White) ]
in
let _table = Table_generator.generate_endgames _pieces _n in
()
