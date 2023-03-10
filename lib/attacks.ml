open Int64;;


(* Genere l'ensemble des tableau des positions attaquables par la tour *)
let generate_rook_masks =
    let column = 0x101010101010101L and line = 0xffL in
    let attacks = Array.make 64 0L in
    let rec aux i j =
        let n = 8 * i + j in
        attacks.(n) <- logor (shift_left line (8*i)) (shift_left column j);
        (* Supprime la case ou se trouve la piece *)
        attacks.(n) <- logand attacks.(n) (lognot (shift_left 1L n)); 
        (* Supprime les bords si la case n'est pas sur un bord,
           reduit le nombre de positions possibles *)
        if i != 0 then
            attacks.(n) <- logand attacks.(n) (lognot line);
        if i != 7 then
            attacks.(n) <- logand attacks.(n) (lognot (shift_left line (8*7)));
        if j != 0 then
            attacks.(n) <- logand attacks.(n) (lognot column);
        if j != 7 then
            attacks.(n) <- logand attacks.(n) (lognot (shift_left column 7));
        
        match (i, j) with
        |(7, 7) -> attacks
        |(i, 7) -> aux (i+1) 0
        |(i, j) -> aux i (j+1)
    in aux 0 0
;;


(* Genere l'ensemble des blocker boards pour un masque donné *)
let generate_blockers mask =
    (* Tableau de taille n=2^p où p est le nombre de 1 *)
    let n = to_int (shift_left 1L (Utils.count_ones mask)) in
    let blockers = Array.make n mask in 
    
    (* Crée un bloqueur unique basé sur le nombre donné en entrée
       0 <= x < n pour avoir toutes les combinaisons possibles *)
    let rec generate x =
        let index = ref 0 in 
        for k = 0 to 63 do
            if (Utils.get_nth blockers.(x) k) = 1L then (
                if (Utils.get_nth (of_int x) !index) = 0L then
                    blockers.(x) <- Utils.clear_nth blockers.(x) k; 
                index := !index + 1 
            )
        done;
        match x with
        |0 -> blockers
        |_ -> generate (x - 1)
    in generate (n - 1)
;;


(* Genere un hash unique par brut force*)