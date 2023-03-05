open Chess_engine;;

let m = Attacks.generate_rook_masks;;
let a = m.(0);;
print_endline (Int64.to_string a);;
let b = Attacks.generate_blockers a;;
print_endline (Int64.to_string b.(0));;
