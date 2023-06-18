open Utils;;
open Solver;;

let (n, b) = (Minimax.minimax ({Board.study_board with iswhite = true}) 1) in
print_endline (string_of_int n);
Board.print_board b;;
