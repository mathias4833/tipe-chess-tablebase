open Utils
open Endgame
open Graphics

let _ =
  let _n = 100 in
  let _pieces = [ (Board.R, Board.White) ] in
  let _table = Table_generator.generate_endgames _pieces _n in
  Density_map.save_to_image _table 231 256
