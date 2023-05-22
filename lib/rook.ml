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
    |0 -> ((!combination)::acc)
    |_ -> generate_combinations_aux bitboard (x-1) ((!combination)::acc)
  in generate_combinations_aux bitboard (n-1) [] 
;;


(* Genere le masque associe aux coordonnees i j *)
let generate_mask i j =
  let n = 8 * i + j in
  let mask = ref 0L in
  (* Remplace la ligne et la colonne ou se trouve la piece par des 1 *)
  mask := logor (shift_left Board.line (8*i)) (shift_left Board.column j);

  (* Supprime la case ou se trouve la piece *)
  mask := logand !mask (lognot (shift_left 1L n)); 

  (* Supprime les bords si la case n'est pas sur un bord, 
    reduit le nombre de positions possibles *)
  if i != 0 then
    mask := logand !mask (lognot Board.line);
  if i != 7 then
    mask := logand !mask (lognot (shift_left Board.line (8*7)));
  if j != 0 then
    mask := logand !mask (lognot Board.column);
  if j != 7 then
    mask := logand !mask (lognot (shift_left Board.column 7));
  
  !mask
;;

(* Genere le bitboard contenant tous les bloqueurs sauf les plus pres de la piece *)
let generate_blockers_from_nearest i j l1 l2 c1 c2 =
  let blockers = ref 0L in
  for n = 1 to l1 - 1 do
    blockers := logor !blockers (Utils.create_board n j);
  done;
  for n = l2 + 1 to 6 do
    blockers := logor !blockers (Utils.create_board n j)
  done;
  for n = 1 to c1 - 1 do
    blockers := logor !blockers (Utils.create_board i n)
  done;
  for n = c2 + 1 to 6 do
    blockers := logor !blockers (Utils.create_board i n)
  done;
  !blockers
;;

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)
let generate_blockers i j =
  let mask = generate_mask i j in 
  let blockers_list = ref [] in

  let start_line = if i = 0 then -1 else 0 in
  let stop_line = if i = 7 then 8 else 7 in
  let start_column = if j = 0 then -1 else 0 in
  let stop_column = if j = 7 then 8 else 7 in
  
  for l1 = start_line to (i-1) do
    for l2 = stop_line downto (i+1) do
      for c1 = start_column to (j-1) do
        for c2 = stop_column downto (j+1) do
          let nearest_blockers = logand mask (List.fold_left logor 0L [
            Utils.create_board l1 j;
            Utils.create_board l2 j;
            Utils.create_board i c1;
            Utils.create_board i c2]) in
          
          let full_blockers = generate_blockers_from_nearest i j l1 l2 c1 c2 in
          let accessible_mask = logand mask (lognot (logor nearest_blockers full_blockers)) in

          (* Liste contenant les bloqueurs possibles *)
          let blockers =
            let rec aux l acc =
              match l with
              |[] -> acc
              |h::t -> aux t ((logor h nearest_blockers)::acc)
            in aux (generate_combinations full_blockers) []
          in
          
          blockers_list := (accessible_mask, blockers)::!blockers_list
        done;
      done;
    done;
  done;

  (mask, !blockers_list)
;;


(* TODO: Commenter ! *)
(* Creation d'un tableau de dictionnaires contenant positions accessible *)
let generate_possible_cases () =
  (* Cree le dictionnaire bloqueurs / cases accessibles *)
  let create_hashmap n =
    let i = n / 8 and j = n mod 8 in
    let (_, all_blockers) = generate_blockers i j in (* Ensemble des bloqueurs *)
    let hashmap = Hashtbl.create 1024 in (* Dictionnaire vide *)

    let rec aux1 l accessible hashmap =
      match l with
      |[] -> hashmap
      |h::t -> (
        Hashtbl.add hashmap h accessible;
        aux1 t accessible hashmap
      )
    and aux2 l hashmap =
      match l with
      |[] -> hashmap
      |(accessible, blockers)::t -> aux2 t (aux1 blockers  accessible hashmap)
    in aux2 all_blockers hashmap
  in Array.init 64 create_hashmap 
;;


Random.self_init ();;

(* Creation du nombre magique *)
let rec generate_magic (mask, blocker_list) =
  let magic = Random.int64 (max_int) in
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
      (* On renvoie true si la valeur associe a l'indice est le meme *)
      match (Hashtbl.find index_map magic_index) = accessible with
      |true -> is_magic accessible t
      |_ -> false
    )
  in let rec check_magic l =
    match l with
    |[] -> (
      print_endline (string_of_int (Hashtbl.length index_map));
      if (Hashtbl.length index_map < 1024) then
        (magic, index_map) (* Tous les elements verfient is_magic *)
      else
        generate_magic (mask, blocker_list)
    )
    |(accessible, blockers)::t when is_magic accessible blockers -> check_magic t (* Le nombre verifie is_magic, on continue*)
    |_ -> generate_magic (mask, blocker_list) (* On essaie un nouveau nombre *)
  in check_magic blocker_list
;;
