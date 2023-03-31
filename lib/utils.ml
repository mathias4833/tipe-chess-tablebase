open Int64;;

(* Renvoie la valeur du nth bit *)
let get_nth x n =
    logand (shift_right x n) 1L  
;;

(* Change la valeur du nth bit par 0 *)
let clear_nth x n =
    logand x (lognot (shift_left 1L n))
;;

(* Renvoie le bitboard en echangeant la valeur du lsb *)
(* LS-Bit-Reset *)
let pop_lsb x =
   logand x (sub x 1L)
;;

(* Effectue un bitscan pour récuperer l'indice du lsb *)
let bruijn_sequence = [|
    0;  1; 48;  2; 57; 49; 28;  3;
   61; 58; 50; 42; 38; 29; 17;  4;
   62; 55; 59; 36; 53; 51; 43; 22;
   45; 39; 33; 30; 24; 18; 12;  5;
   63; 47; 56; 27; 60; 41; 37; 16;
   54; 35; 52; 21; 44; 32; 23; 11;
   46; 26; 40; 15; 34; 20; 31; 10;
   25; 14; 19;  9; 13;  8;  7;  6
|];;

(* Precondition: x est non nul *)
let bitscan_forward x =
  let magic = 0x03f79d71b4cb0a89L in
  let n = mul (logand x (neg x)) magic in
  print_int (to_int n);
  print_endline ("\n" ^ to_string (mul x magic));
  bruijn_sequence.(to_int (shift_right n 58))
;;


(* Isole le lsb *)
(* Voir: LS-Bit-Isolation *)
let isolate_lsb x =
  logand x (neg x)
;;

(* Renvoie l'indice du lsb *)
(* precondition: x est non nul *)
let get_lsb x =
    bitscan_forward (isolate_lsb x)
;;

(* lent mais fonctionne *)
let bitscan_forward2 x =
  let rec get_lsb_aux x acc =
    match x with
    |1L -> acc
    |_ -> get_lsb_aux (shift_right x 1) (acc+1)
  in match x with
  |0L -> 0
  |_ -> get_lsb_aux x 1
;;

let get_lsb2 x =
  bitscan_forward2 (isolate_lsb x)
;;

(* Compte le nombre de 1 *)
let rec count_ones x =
    match x with
    |0L -> 0
    |x when (logand x 1L) = 1L -> 1 + count_ones (shift_right x 1)
    |_ -> count_ones (shift_right x 1)
;;