open Bigarray
open Utils

(** [open_table pieces] ouvre et initialise une table de fin de partie.
    @param pieces Liste des pièces présentes.
    @return (descripteur de fichier, table mappee). *)
let open_table pieces =
  (* Calcul de la taille de la table en fonction du nombre de pieces *)
  let size = 462 * (1 lsl (6 * List.length pieces)) * 2 in
  let file_descr =
    Unix.openfile "endgame.table"
      [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC ]
      0o644
  in
  let table = Unix.map_file file_descr int8_unsigned c_layout true [| size |] in
  Genarray.fill table 0;
  (file_descr, array1_of_genarray table)

(** [close_table file_descr] ferme le fichier associe au descripteur [file_descr].
    @param file_descr Descripteur de fichier à fermer. *)
let close_table file_descr = Unix.close file_descr

(** [color_to_number color] convertit une couleur en nombre.
    @param color La couleur à convertir ([Board.White] ou [Board.Black]).
    @return 1 pour [Board.White], 0 pour [Board.Black]. *)
let color_to_number = function Board.White -> 1 | Black -> 0

(** [number_to_color n] convertit un nombre en couleur.
    @param n Le nombre à convertir (1 ou 0).
    @return [Board.White] pour 1, [Board.Black] pour 0, sinon échoue. *)
let number_to_color = function
  | 1 -> Board.White
  | 0 -> Board.Black
  | _ -> failwith "Indice invalide"

(** [white_king_to_number n] convertit une position d'un roi en sa position relative dans le cadrant inferieur gauche.
    @param n L'indice du roi (entre 0 et 63).
    @return L'indice du roi dans le cadrant, entre 0 et 9. Echoue si le roi n'est pas dans le cadrant. *)
let white_king_to_number = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 9 -> 4
  | 10 -> 5
  | 11 -> 6
  | 18 -> 7
  | 19 -> 8
  | 27 -> 9
  | _ -> failwith "Indice invalide"

(** [number_to_white_king n] convertit une position relative d'un roi dans le cadrant inférieur gauche en sa position absolue.
    @param n L'indice du roi dans le cadrant (entre 0 et 9).
    @return L'indice du roi absolu (entre 0 et 63). Echoue si l'indice est invalide. *)
let number_to_white_king = function
  | 0 -> 0
  | 1 -> 1
  | 2 -> 2
  | 3 -> 3
  | 4 -> 9
  | 5 -> 10
  | 6 -> 11
  | 7 -> 18
  | 8 -> 19
  | 9 -> 27
  | _ -> failwith "Indice invalide"

(* Associe les deux rois a un indice entre 0 et 461 *)

(** [kings_lookup_table] crée une table d'association d'un indice (entre 0 et 461) a la position des deux rois sur l'échiquier.
    Associe un indice à la position des deux rois sur l'échiquier, en ne considérant que les positions 
    dans le quadrant inférieur gauche où le roi blanc est placé.
    @return Un tuple de deux matrices d'indices:
            - La première matrice associe a la position des deux rois un indice
            - La deuxième matrice associe a un indice la position des deux rois *)
let kings_lookup_table =
  let index_of_couple = Array.make_matrix 10 64 (-1) in
  let couple_of_index = Array.make 462 (-1, -1) in
  let c = ref 0 in
  for p = 0 to 9 do
    let n = number_to_white_king p in
    let i, j = Bitboard.coord_of_index n in
    for m = 0 to 63 do
      let k, l = Bitboard.coord_of_index m in
      (* Si les rois ne se touchent pas et si le roi noir et sous la diagonale quand le roi blanc est dessus*)
      if (abs (i - k) > 1 || abs (j - l) > 1) && (i <> j || k <= l) then (
        index_of_couple.(p).(m) <- !c;
        couple_of_index.(!c) <- (n, m);
        incr c)
    done
  done;
  (index_of_couple, couple_of_index)

(** [kings_index_of_couple i j] retourne l'indice associé à une paire de positions de rois.
    @param i Indice de la position du roi blanc.
    @param j Indice de la position du roi noir.
    @return L'indice associé à la paire de positions de rois. *)
let kings_index_of_couple i j =
  let n = white_king_to_number i in
  (fst kings_lookup_table).(n).(j)

(** [kings_couple_of_index n] retourne la paire de positions de rois associée à un indice.
    @param n L'indice associé à la paire de positions de rois.
    @return La paire de positions de rois associée à l'indice [n]. *)
let kings_couple_of_index n = (snd kings_lookup_table).(n)

(** [board_to_number board pieces] convertit une configuration de plateau en un indice unique.
    @param board Le plateau d'échecs.
    @param pieces La liste des pièces présentes sur le plateau.
    @return Un indice unique représentant la configuration du plateau, entre 0 et 462*64^(pieces)*2 *)
let board_to_number (board : Board.chessboard) pieces =
  let rec aux acc king_square = function
    | [] -> (2 * acc) + color_to_number board.color
    | h :: t ->
        let b = Board.get_bitboard board h in
        let n = if b = 0L then king_square else Bitboard.get_lsb b in
        aux ((acc * 64) + n) king_square t
  in
  let n = Bitboard.get_lsb (Board.get_bitboard board (Board.K, Board.White)) in
  let m = Bitboard.get_lsb (Board.get_bitboard board (Board.K, Board.Black)) in
  aux (kings_index_of_couple n m) n pieces

(** [number_to_board num pieces] convertit un indice en une configuration de plateau.
    @param num L'indice représentant la configuration du plateau.
    @param pieces La liste des pièces présentes sur le plateau.
    @return Le plateau d'échecs correspondant à l'indice donné. *)
let number_to_board num pieces =
  let rec add_pieces acc num king_square = function
    | [] -> Transformations.normalize_board acc pieces
    | h :: t ->
        let q = num / 64 in
        let r = num mod 64 in
        if r = king_square then add_pieces acc q king_square t
        else
          let b = Bitboard.set_nth 0L r in
          add_pieces (Board.set_bitboard acc b h) q king_square t
  in
  let color = number_to_color (num mod 2) in
  let kings_index = num lsr (1 + (6 * List.length pieces)) in
  let n, m = kings_couple_of_index kings_index in
  let board = Board.empty_board color in
  add_pieces
    (Board.set_bitboard
       (Board.set_bitboard board (Bitboard.set_nth 0L n) (Board.K, Board.White))
       (Bitboard.set_nth 0L m) (Board.K, Board.Black))
    (num / 2) n (List.rev pieces)
