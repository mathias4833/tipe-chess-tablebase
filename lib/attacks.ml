open Int64;;

(* Genere la liste des combinaisons de 0 et de 1 à partir d'un nombre donne *)
let generate_combinations bitboard =
  (* Nombre de combinaisons possible, n = 2^p avec p le nombre de 1 *)
  let n = to_int (shift_left 1L (Utils.count_ones bitboard)) in
  (* Cree la combinaison associee au nombre, 0 <= x < n pour avoir toutes les combinaisons *)
  let rec generate_combinations_aux bitboard x acc =
    let index = ref 0 in
    let combination = ref bitboard in
    (* Parcours du nombre pour remplacer les 1 par des 0 *)
    for k = 0 to 63 do
      (* Si le k-ieme bit de !combination est un 1 alors que celui de x est un 0 *)
      if (Utils.get_nth !combination k) = 1L then (
        if (Utils.get_nth (of_int x) !index) = 0L then (
          (* On remplace le k-ieme bit par un 0 *)
          combination := Utils.clear_nth !combination k;
        );
        index := !index + 1
      );
    done;
    match x with
    |0 -> acc
    |_ -> generate_combinations_aux bitboard (x-1) ((!combination)::acc)
  in generate_combinations_aux bitboard (n-1) [] 
;;

(* Genere le masque associe aux coordonnees i j *)
let generate_mask i j =
  let column = 0x101010101010101L and line = 0xffL in

  let n = 8 * i + j in
  let mask = ref 0L in
  (* Remplace la ligne et la colonne ou se trouve la piece par des 1 *)
  mask := logor (shift_left line (8*i)) (shift_left column j);

  (* Supprime la case ou se trouve la piece *)
  mask := logand !mask (lognot (shift_left 1L n)); 

  (* Supprime les bords si la case n'est pas sur un bord, 
    reduit le nombre de positions possibles *)
  if i != 0 then
    mask := logand !mask (lognot line);
  if i != 7 then
    mask := logand !mask (lognot (shift_left line (8*7)));
  if j != 0 then
    mask := logand !mask (lognot column);
  if j != 7 then
    mask := logand !mask (lognot (shift_left column 7));
  
  !mask
;;

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)
let generate_blockers i j =
  let column = 0x101010101010101L and line = 0xffL in
  let mask = generate_mask i j in

  let rec generate_blockers_aux l1 l2 c1 c2 acc =
    (* Masque des cases accessible depuis (i, j) *)
    let accessible = ref mask in 

    (* On enleve les pieces en dessous de l1 *)
    for k = 0 to l1 do
      accessible := logand !accessible (lognot (shift_left line (8*k)));
    done;
    (* On enleve les pieces au dessus de l2 *)
    for k = l2 to 7 do
      accessible := logand !accessible (lognot (shift_left line (8*k)));
    done;
    (* On enleve les pieces a gauche de c1 *)
    for k = 0 to c1 do
      accessible := logand !accessible (lognot (shift_left column k));
    done;
    (* On enleve les pieces a droite de c2 *)
    for k = c2 to 7 do
      accessible := logand !accessible (lognot (shift_left column k));
    done;
          
    (* On recupere les cases innacessibles *)
    let inaccessible = logand mask (lognot !accessible) in

    (* On cree un masque avec les coordonnees l1 l2 c1 c2 *)
    let boundaries_mask = 
      let temp = ref 0L in
      if l1 > -1 then
        temp := logor !temp (logand mask (shift_left line (8*l1)));
      if l2 < 8 then
        temp := logor !temp (logand mask (shift_left line (8*l2)));
      if c1 > -1 then
        temp := logor !temp (logand mask (shift_left column c1));
      if c2 < 8 then
        temp := logor !temp (logand mask (shift_left line (c2)));
      !temp
    in

    (* Liste contenant tous les bloqueurs possible,  *)
    let blockers = 
      (* On ajoute recursivement les limites *)
      let rec aux l acc =
        match l with
        |[] -> acc
        |h::t -> aux t ((logor h (logand inaccessible boundaries_mask))::acc)
      in 
      (* Toutes les combinaisons sans les cases limites *)
      aux (generate_combinations (logand inaccessible (lognot boundaries_mask))) []
    in
    Board.print_bitboard mask;

    (*TODO: pas coder en dur les possibilites
      Actuellement dans ce cas car indices peuvent sortir du bord dans le cas d'une case sur le cote *)
    match (l1, l2, c1, c2) with
    |(0, 7, 0, 7)|(0, 7, 0, 8)|(0, 7, -1, 7)|(0, 7, -1, 8)|(0, 8, 0, 7)|(0, 8, 0, 8)|(0, 8, -1, 7)|(0, 8, -1, 8)|(-1, 7, 0, 7)|(-1, 7, 0, 8)|(-1, 7, -1, 7)|(-1, 7, -1, 8)|(-1, 8, 0, 7)|(-1, 8, 0, 8)|(-1, 8, -1, 7)|(-1, 8, -1, 8) -> acc
    |(_, 7, 0, 7)|(_, 7, 0, 8)|(_, 7, -1, 7)|(_, 7, -1, 8)|(_, 8, 0, 7)|(_, 8, 0, 8)|(_, 8, -1, 8)|(_, 8, -1, 7) -> generate_blockers_aux (l1 - 1) (i+1) (j-1) (j+1) ((!accessible, blockers)::acc)
    |(_, _, 0, 7)|(_, _, 0, 8)|(_, _, -1, 7)|(_, _, -1, 8) -> generate_blockers_aux l1 (l2 + 1) (j-1) (j+1) ((!accessible, blockers)::acc)
    |(_, _, _, 7)|(_, _, _, 8) -> generate_blockers_aux l1 l2 (c1 - 1) (j+1) ((!accessible, blockers)::acc)
    |_ -> generate_blockers_aux l1 l2 c1 (c2 + 1) ((!accessible, blockers)::acc)
  in (mask, generate_blockers_aux (i-1) (i+1) (j-1) (j+1) [])
;;



(* Genere le tableau de l'ensemble des bloqueurs, tableau de 64 couples (a, b)
   avec a le masque des positions accessibles, et b la liste de tous les bloqueurs
   et position resultante *)
let generate_rook_attacks () =
  let board = Array.make 64 (0L, []) in
  
  let rec generate_rook_table_aux i j =
    let n = 8*i + j in
    board.(n) <- generate_blockers i j;
    match (i, j) with
    |(7, 7) -> board
    |(_, 7) -> generate_rook_table_aux (i+1) 0
    |_ -> generate_rook_table_aux i (j+1)
  in generate_rook_table_aux 0 0
;;


Random.self_init ();;

(* Creation du nombre magique *)
let rec generate_magic (mask, blocker_list) =
  let magic = Random.int64 (max_int) in
  print_endline (to_string magic);
  (* Nombre de combinaisons, n = 2^p avec p le nombre de 1 *)
  let n = to_int (shift_left 1L (Utils.count_ones mask)) in
  
  let index_map = Hashtbl.create 1000 in
  
  (* Verifie si le nombre est bien magique *)
  let rec is_magic accessible blockers =
    match blockers with
    |[] -> true
    |h::t -> (
      (* Cree l'indice associee au blocker board *)
      let magic_index = shift_right (mul h magic) n in
      (* Si l'indice n'existe pas encore, on l'ajoute *)
      if not (Hashtbl.mem index_map magic_index) then (
        Hashtbl.add index_map magic_index accessible;
      );
      Board.print_bitboard (h);
      (* On renvoie true si la valeur associe a l'indice est le meme *)
      match (Hashtbl.find index_map magic_index) = accessible with
      |true -> is_magic accessible t
      |_ -> false
    )
  in let rec check_magic l =
    match l with
    |[] -> magic (* Tous les elements verfient is_magic *)
    |(accessible, blockers)::t when is_magic accessible blockers -> check_magic t (* Le nombre verifie is_magic, on continue*)
    |_ -> generate_magic (mask, blocker_list) (* On essaie un nouveau nombre *)
  in check_magic blocker_list
;;

let generate_all_magics () =
  let attacks = generate_rook_attacks () in
  Array.init 1 (fun n -> generate_magic attacks.(n))
;;
