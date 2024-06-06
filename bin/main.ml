open Utils
open Endgame
open Graphics

let _ =
  let _n = 100 in
  let _pieces = [ (Board.R, Board.White) ] in
  let _table = Table_generator.generate_endgames _pieces _n in
  Image.save_to_image _table _pieces 231 256
