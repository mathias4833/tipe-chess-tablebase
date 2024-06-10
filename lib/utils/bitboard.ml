open Int64

(* Colonne et ligne de 1s *)
let column = 0x101010101010101L
let line = 0xffL

(** [index_of_coord i j] calcule l'indice correspondant à une paire de coordonnées (i, j).
    @param i L'indice de la ligne.
    @param j L'indice de la colonne.
    @return L'indice correspondant à la position (i, j) entre 0 et 63 *)
let index_of_coord i j = (i * 8) + j

(** [coord_of_index n] calcule les coordonnées correspondant à un indice n.
    @param n L'indice sur le plateau d'échecs.
    @return Un couple (i, j) représentant les coordonnées correspondant à l'indice n *)
let coord_of_index n = (n / 8, n mod 8)

(** [get_nth x n] récupère le n-ième bit d'un entier x.
    @param x L'entier dont on veut extraire le n-ième bit.
    @param n L'indice du bit à récupérer.
    @return valeur du n-ieme bit (0 ou 1) *)
let get_nth x n = logand (shift_right_logical x n) 1L

(** [set_nth x n] met à 1 le n-ieme bit d'un entier x.
    @param x L'entier dans lequel on veut définir le n-ième bit.
    @param n L'indice du bit à mettre à 1.
    @return L'entier résultant après avoir défini le n-ième bit de x à 1. *)
let set_nth x n = logor x (shift_left 1L n)

(** [clear_nth x n] met à 0 le n-ième bit d'un entier x.
    @param x L'entier dans lequel on veut mettre à 0 le n-ième bit.
    @param n L'indice du bit à mettre à 0.
    @return L'entier résultant après avoir mis à 0 le n-ième bit de x. *)
let clear_nth x n = logand x (lognot (shift_left 1L n))

(** [pop_lsb x] supprime le bit de poids faible d'un entier x.
    @param x L'entier dont on veut supprimer le bit de poids faible.
    @return L'entier résultant après avoir supprimé le bit de poids faible de x. *)
let pop_lsb x = logand x (sub x 1L)

(** [get_lsb x] récupère l'indice du bit de poids faible d'un entier x.
    @param x L'entier dont on veut récupérer l'indice du bit de poids faible.
    @return L'indice du bit de poids faible de x, ou 0 si x est égal à zéro. *)
let get_lsb x =
  let rec get_lsb_aux x acc =
    match logand x 1L with
    | 1L -> acc
    | _ -> get_lsb_aux (shift_right_logical x 1) (acc + 1)
  in
  match x with 0L -> 0 | _ -> get_lsb_aux x 0

(** [count_ones x] compte le nombre de bits à 1 dans un entier x.
    @param x L'entier dont on veut compter le nombre de bits à 1.
    @return Le nombre de bits à 1 dans x. *)
let rec count_ones x =
  match x with
  | 0L -> 0
  | x when logand x 1L = 1L -> 1 + count_ones (shift_right_logical x 1)
  | _ -> count_ones (shift_right_logical x 1)

(** [from_coordinate i j] crée un bitboard avec un seul bit positionné en (i, j).
    @param i L'indice de la ligne.
    @param j L'indice de la colonne.
    @return Un bitboard avec un seul bit positionné en (i, j), ou 0L si les coordonnées sont hors de la grille *)
let from_coordinate i j =
  if i < 0 || i > 7 || j < 0 || j > 7 then 0L
  else shift_left 1L (index_of_coord i j)

(** [from_coordinates l] crée un bitboard avec plusieurs bits en appelant recursivement [from_coordinate i j].
    Les coordonnées invalides sont ignorées.
    @param l La liste de paires (i, j) représentant les coordonnées des bits à positionner.
    @return Un bitboard avec des bits positionnés aux coordonnées spécifiées dans la liste, ou un bitboard 
    vide (0L) si toutes les coordonnées sont invalides. *)
let from_coordinates l =
  let rec from_coordinates_aux l acc =
    match l with
    | [] -> acc
    | (i, j) :: t when i < 0 || i > 7 || j < 0 || j > 7 ->
        from_coordinates_aux t acc
    | (i, j) :: t -> from_coordinates_aux t (set_nth acc (index_of_coord i j))
  in
  from_coordinates_aux l 0L

(** [print_board b] affiche un bitboard.
    @param b le bitboard à afficher. *)
let print_board b =
  (* Print la ligne i du bitboard *)
  let rec line i b =
    match (i, b) with
    | i, _ when i < 0 -> "\n"
    | _, b when logand b 1L = 1L -> "1" ^ line (i - 1) (shift_right_logical b 1)
    | _ -> "." ^ line (i - 1) (shift_right_logical b 1)
  (* Print le bitboard ligne par ligne *)
  and print_bitboard_aux i b =
    match (i, b) with
    | i, _ when i < 0 -> ""
    | _ -> print_bitboard_aux (i - 1) (shift_right_logical b 8) ^ line 7 b
  in
  print_string ("--------\n" ^ print_bitboard_aux 7 b ^ "--------\n")

(** [print_list_board l] affiche une liste de bitboards.
    @param l La liste de bitboards à afficher. *)
let rec print_list_board l =
  match l with
  | [] -> ()
  | h :: t ->
      print_board h;
      print_string "\n";
      print_list_board t

(** [generate_combinations bitboard] génère une liste de bitboards en remplaçant chaque 1 dans le bitboard donné par un 0 ou 1.
    @param bitboard Le bitboard à partir duquel générer les combinaisons.
    @return Une liste de bitboards représentant toutes les combinaisons possibles. *)
let generate_combinations bitboard =
  (* Nombre de combinaisons possible, n = 2^p avec p le nombre de 1 *)
  let n = to_int (shift_left 1L (count_ones bitboard)) in
  (* Cree la combinaison associee au nombre, 0 <= x < n pour avoir toutes les combinaisons *)
  let rec generate_combinations_aux bitboard x acc =
    let index = ref 0 in
    let combination = ref bitboard in
    (* Parcours du nombre pour remplacer les 1 par des 0 *)
    for k = 0 to 63 do
      (* Si le k-ieme bit de !combination est un 1 alors que celui de x est un 0 *)
      if get_nth !combination k = 1L then (
        if get_nth (of_int x) !index = 0L then
          (* On remplace le k-ieme bit par un 0 *)
          combination := clear_nth !combination k;
        index := !index + 1)
    done;
    match x with
    | 0 -> !combination :: acc
    | _ -> generate_combinations_aux bitboard (x - 1) (!combination :: acc)
  in
  generate_combinations_aux bitboard (n - 1) []
