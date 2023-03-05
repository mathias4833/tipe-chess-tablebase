open Int64;;

(* Renvoie la valeur du nth bit *)
let get_nth x n =
    logand (shift_right x n) 1L  
;;

(* Change la valeur du nth bit par 0 *)
let clear_nth x n =
    logand x (lognot (shift_left 1L n))
;;

(* Renvoie l'indice du lsb *)
let rec get_lsb x =
    match x with
    |0L -> failwith "Le nombre est nul"
    |x when (logand x 1L) = 1L -> 0
    |_ -> 1 + get_lsb (shift_right x 1)
;;

(* Renvoie le bitboard en echangeant la valeur du lsb *)
let pop_lsb x =
    let n = get_lsb x in
    shift_left (shift_right x (n+1)) (n+1)
;;

(* Compte le nombre de 1 *)
let rec count_ones x =
    match x with
    |0L -> 0
    |x when (logand x 1L) = 1L -> 1 + count_ones (shift_right x 1)
    |_ -> count_ones (shift_right x 1)
;;
