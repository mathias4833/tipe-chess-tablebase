open Int64
open Utils

(** [generate_mask i j] génère un bitboard représentant les cases attaquées par une tour à une position donnée.
    @param i Ligne de la tour (entre 0 et 7).
    @param j Colonne de la tour (entre 0 et 7).
    @return Le bitboard représentant les cases attaquées par la reine. *)
let generate_mask i j =
  let n = (8 * i) + j in
  let mask = ref 0L in
  (* Remplace la ligne et la colonne ou se trouve la piece par des 1 *)
  mask :=
    logor (shift_left Bitboard.line (8 * i)) (shift_left Bitboard.column j);

  (* Supprime la case ou se trouve la piece *)
  mask := logand !mask (lognot (shift_left 1L n));

  (* Supprime les bords si la case n'est pas sur un bord,
     reduit le nombre de positions possibles *)
  if i != 0 then mask := logand !mask (lognot Bitboard.line);
  if i != 7 then
    mask := logand !mask (lognot (shift_left Bitboard.line (8 * 7)));
  if j != 0 then mask := logand !mask (lognot Bitboard.column);
  if j != 7 then mask := logand !mask (lognot (shift_left Bitboard.column 7));
  !mask

(** [table_mask] crée une table de bitboards représentant les cases attaquées par une tour depuis chaque position sur l'échiquier.
    @return Un tableau de 64 bitboards. *)
let table_mask =
  let create_table_mask n =
    let i, j = Bitboard.coord_of_index n in
    generate_mask i j
  in
  Array.init 64 create_table_mask

(* Genere le bitboard contenant tous les bloqueurs sauf les plus pres de la piece *)
let generate_blockers_from_nearest i j l1 l2 c1 c2 =
  let blockers = ref 0L in
  for n = 1 to l1 - 1 do
    blockers := logor !blockers (Bitboard.from_coordinate n j)
  done;
  for n = l2 + 1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate n j)
  done;
  for n = 1 to c1 - 1 do
    blockers := logor !blockers (Bitboard.from_coordinate i n)
  done;
  for n = c2 + 1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate i n)
  done;
  !blockers

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)

(** [generate_blockers i j] génère une liste de bitboards représentant les positions bloquées par des pièces ennemies.
    @param i Ligne de la tour.
    @param j Colonne de la tour.
    @return Une liste de tuples (bitboard représentant les cases accessibles, liste de bitboards représentant les positions des bloqueurs *)
let generate_blockers i j =
  let mask = table_mask.(Bitboard.index_of_coord i j) in
  let blockers_list = ref [] in

  let start_line = if i = 0 then -1 else 0 in
  let stop_line = if i = 7 then 8 else 7 in
  let start_column = if j = 0 then -1 else 0 in
  let stop_column = if j = 7 then 8 else 7 in

  for l1 = start_line to i - 1 do
    for l2 = stop_line downto i + 1 do
      for c1 = start_column to j - 1 do
        for c2 = stop_column downto j + 1 do
          let nearest_blockers =
            List.fold_left logor 0L
              [
                Bitboard.from_coordinate l1 j;
                Bitboard.from_coordinate l2 j;
                Bitboard.from_coordinate i c1;
                Bitboard.from_coordinate i c2;
              ]
          in

          let full_blockers = generate_blockers_from_nearest i j l1 l2 c1 c2 in
          let accessible_mask =
            logand (logor mask nearest_blockers) (lognot full_blockers)
          in

          (* Liste contenant les bloqueurs possibles *)
          let blockers =
            let rec aux l acc =
              match l with
              | [] -> acc
              | h :: t -> aux t (logand mask (logor h nearest_blockers) :: acc)
            in
            aux (Bitboard.generate_combinations full_blockers) []
          in

          blockers_list := (accessible_mask, blockers) :: !blockers_list
        done
      done
    done
  done;

  !blockers_list

(** [table_moves] crée un tableau de dictionnaires associant les bitboards des bloqueurs aux bitboards des cases accessibles.
    @return Un tableau de 64 dictionnaires. *)
let table_moves =
  (* Cree le dictionnaire bloqueurs / cases accessibles *)
  let create_hashmap n =
    let i = n / 8 and j = n mod 8 in
    let all_blockers = generate_blockers i j in
    (* Ensemble des bloqueurs *)
    let hashmap = Hashtbl.create 1024 in

    (* Dictionnaire vide *)

    (* A chaque bloqueur on associe le bitboard des cases accesibles *)
    let rec aux1 l accessible hashmap =
      match l with
      | [] -> hashmap
      | h :: t ->
          Hashtbl.add hashmap h accessible;
          aux1 t accessible hashmap
    and aux2 l hashmap =
      match l with
      | [] -> hashmap
      | (accessible, blockers) :: t -> aux2 t (aux1 blockers accessible hashmap)
    in
    aux2 all_blockers hashmap
  in
  Array.init 64 create_hashmap

(** [moves_from_board ally whole n] génère les coups possibles pour une case donnée.
    @param ally Bitboard des pièces alliées.
    @param whole Bitboard des pièces sur tout le plateau.
    @param n Indice de la position de la tour sur l'échiquier (entre 0 et 63).
    @return Bitboard des coups possibles pour la pièce. *)
let moves_from_board ally whole n =
  (* Bitboard contenant l'ensemble des pieces bloquantes *)
  let blockerboard = logand whole table_mask.(n) in
  logand (Hashtbl.find table_moves.(n) blockerboard) (lognot ally)

(** [generate_moves chessboard] génère les coups possibles pour toutes les tours.
    @param chessboard Le plateau d'échecs.
    @return Liste des coups possibles pour les tours. *)
let generate_moves chessboard =
  let ally = Board.get_ally_board chessboard in
  let whole = Board.get_whole_board chessboard in

  (* Fonction auxiliaire pour parcourir l'ensemble des tours *)
  let rec generate_moves_aux board acc =
    match board with
    | 0L -> acc
    | _ ->
        (* Indice de la tour *)
        let n = Bitboard.get_lsb board in
        let moves = moves_from_board ally whole n in
        generate_moves_aux (Bitboard.pop_lsb board)
          (Move.add_moves_to_list R n moves acc)
  in
  generate_moves_aux (Board.get_ally_bitboard chessboard R) []

(** [unmoves_from_board whole n] génère les annulations de coups possibles pour une case donnée.
    @param whole Bitboard des pièces sur tout le plateau.
    @param n Indice de la position de la tour sur l'échiquier (entre 0 et 63).
    @return Bitboard des annulations de coups possibles pour la pièce. *)
let unmoves_from_board whole n =
  let blockerboard = logand whole table_mask.(n) in
  logand (Hashtbl.find table_moves.(n) blockerboard) (lognot whole)

(** [generate_unmoves chessboard pieces] génère les annulations de coups possibles pour toutes les tours.
    @param chessboard Le plateau d'échecs.
    @param pieces Liste des pieces présentes sur le plateau.
    @return Liste des annulations de coups possibles pour les tours. *)
let generate_unmoves chessboard pieces =
  let whole = Board.get_whole_board chessboard in
  let rec generate_unmoves_aux acc = function
    | 0L -> acc
    | b ->
        let n = Bitboard.get_lsb b in
        let unmoves = unmoves_from_board whole n in
        generate_unmoves_aux
          (Move.add_unmoves_to_list R n acc chessboard pieces unmoves)
          (Bitboard.pop_lsb b)
  in
  generate_unmoves_aux [] (Board.get_ally_bitboard chessboard R)
