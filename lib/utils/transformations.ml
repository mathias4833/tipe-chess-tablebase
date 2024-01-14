type transformation =
  | Symmetry
  | Rotation
  | Transformation of transformation * transformation

let ( << ) f g x = f (g x)

let transform_bitboard f b =
  let rec aux acc = function
    | 0L -> acc
    | b ->
        let i, j = Bitboard.coord_of_index (Bitboard.get_lsb b) in
        let fi, fj = f (i, j) in
        aux
          (Bitboard.set_nth acc (Bitboard.index_of_coord fi fj))
          (Bitboard.pop_lsb b)
  in
  aux 0L b

let transform_board f (b : Board.chessboard) =
  {
    b with
    wpawns = transform_bitboard f b.wpawns;
    wknights = transform_bitboard f b.wknights;
    wbishops = transform_bitboard f b.wbishops;
    wrooks = transform_bitboard f b.wrooks;
    wqueen = transform_bitboard f b.wqueen;
    wking = transform_bitboard f b.wking;
    bpawns = transform_bitboard f b.bpawns;
    bknights = transform_bitboard f b.bknights;
    bbishops = transform_bitboard f b.bbishops;
    brooks = transform_bitboard f b.brooks;
    bqueen = transform_bitboard f b.bqueen;
    bking = transform_bitboard f b.bking;
  }

let transform_move f = function
  | Board.Chessmove (p, n, m) ->
      let ni, nj = f (Bitboard.coord_of_index n) in
      let mi, mj = f (Bitboard.coord_of_index m) in
      let tn = Bitboard.index_of_coord ni nj in
      let tm = Bitboard.index_of_coord mi mj in
      Board.Chessmove (p, tn, tm)
  | m -> m

(* Tourne le bitboard de pi/2 *)
let rotate_counterclockwise (i, j) = (j, 7 - i)

(* Tourne le bitboard de -pi/2 *)
let rotate_clockwise (i, j) = (7 - j, i)

(* Effectue une symetrie verticale *)
let flip_vertical (i, j) = (i, 7 - j)
let flip_horizontal (i, j) = (7 - i, j)

(* Effectue une symetrie diagonale *)
let flip_diagonal (i, j) = (j, i)
let flip_antidiagonal (i, j) = (7 - j, 7 - i)

(* Verifie si l'echiquier est normalise *)
let is_normalized (board : Board.chessboard)
    (pieces : Board.colored_chesspiece list) =
  let get_coord b p =
    let n = Bitboard.get_lsb (Board.get_bitboard b p) in
    Bitboard.coord_of_index n
  in
  let rec aux = function
    | [] -> true
    | h :: t ->
        let i, j = get_coord board h in
        if i = j then aux t else i < j
  in
  match pieces with
  | [] -> true
  | h :: t ->
      let i, j = get_coord board h in
      if j > 3 || i > 3 then false else if i = j then aux t else i < j

(* Tourne l'echiquier pour avoir le roi dans le triangle en bas a gauche de l'echiquier *)
let rec normalize_board (board : Board.chessboard) (move : Board.chessmove)
    (pieces : Board.colored_chesspiece list) =
  if is_normalized board pieces then (board, move)
  else
    let transform_aux f b m = (transform_board f b, transform_move f m) in

    let n = Bitboard.get_lsb board.wking in
    match Bitboard.coord_of_index n with
    (* Le roi est bien place mais l'echiquier n'est pas normalise, donc la piece suivante n'est pas bien placee *)
    | i, j when i < 4 && j < 4 && i <= j ->
        transform_aux flip_diagonal board move
    (* Triangle gauche en bas a gauche *)
    | i, j when i < 4 && j < 4 && i > j ->
        let b, m = transform_aux flip_diagonal board move in
        normalize_board b m pieces
    (* Triangle gauche en bas a droite *)
    | i, j when i < 4 && j >= 4 && i <= 7 - j ->
        let b, m = transform_aux flip_vertical board move in
        normalize_board b m pieces
    (* Triangle droit en bas a droite *)
    | i, j when i < 4 && j >= 4 && i > 7 - j ->
        let b, m = transform_aux rotate_clockwise board move in
        normalize_board b m pieces
    (* Triangle gauche en haut a gauche *)
    | i, j when i >= 4 && j < 4 && i <= 7 - j ->
        let b, m = transform_aux rotate_counterclockwise board move in
        normalize_board b m pieces
    (* Triangle droit en haut a gauche *)
    | i, j when i >= 4 && j < 4 && i > 7 - j ->
        let b, m = transform_aux flip_horizontal board move in
        normalize_board b m pieces
    (* Triangle gauche en haut a droite *)
    | i, j when i >= 4 && j >= 4 && i > j ->
        let b, m = transform_aux rotate_counterclockwise board move in
        normalize_board b m pieces
    (* Triangle droit en haut a droite *)
    | i, j when i >= 4 && j >= 4 && i <= j ->
        let b, m = transform_aux flip_antidiagonal board move in
        normalize_board b m pieces
    | _ -> failwith "Cas impossible"
