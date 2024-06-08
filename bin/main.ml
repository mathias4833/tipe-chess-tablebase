open Utils
open Endgame
open Unix

let () =
  let depth = 100 in
  let pieces = [ (Board.R, Board.White) ] in

  Printf.printf "Creation de la table\n%!";
  let start_time = gettimeofday () in
  let table = Table_generator.generate_endgames pieces depth in
  Printf.printf "Table cree en %fs\n%!" (gettimeofday () -. start_time);

  Printf.printf "Verification de la table\n%!";
  Table_analysis.check_integrity table pieces;
  Printf.printf "Table verifiee.\n%!";

  let board : Board.chessboard =
    {
      wpawns = 0L;
      wknights = 0L;
      wbishops = 0L;
      wrooks = 0x8000000L;
      wqueen = 0L;
      wking = 0x40000L;
      bpawns = 0L;
      bknights = 0L;
      bbishops = 0L;
      brooks = 0L;
      bqueen = 0L;
      bking = 0x200000000000L;
      color = Board.White;
      wcastle = false;
      bcastle = false;
    }
  in
  match Table_analysis.find_checkmate table pieces board with
  | None -> Printf.printf "Pas de mat possible"
  | Some l ->
      Printf.printf "Mat en %d:\n%!" (List.length l - 1);
      Board.print_list_board l
