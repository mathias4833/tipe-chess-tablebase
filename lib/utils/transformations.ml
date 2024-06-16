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

(* Operations de transformation *)
let rotate_counterclockwise (i, j) = (j, 7 - i)
let rotate_clockwise (i, j) = (7 - j, i)
let flip_horizontal (i, j) = (7 - i, j)
let flip_diagonal (i, j) = (j, i)
let flip_antidiagonal (i, j) = (7 - j, 7 - i)

(** [position_key board pieces] associe une liste d'indices aux pièces d'un plateau.
    Les pièces absentes sont associées à l'indice [-1]. *)
let position_key board pieces =
  List.map
    (function Some square -> square | None -> -1)
    (Board.get_piece_positions board pieces)

(* Verifie si l'echiquier est normalise *)
let is_normalized (board : Board.chessboard)
    (pieces : Board.colored_chesspiece list) =
  let white_king = Board.get_bitboard board (Board.K, Board.White) in
  let i, j = Bitboard.coord_of_index (Bitboard.get_lsb white_king) in
  if j > 3 || i > 3 then false
  else if i <> j then i < j
  else
    (* Si les deux rois sont sur l'axe de symétrie, les autres pièces
       permettent de choisir entre le plateau et son symétrique. *)
    let compared_pieces = (Board.K, Board.Black) :: pieces in
    let flipped = transform_board flip_diagonal board in
    position_key board compared_pieces
    <= position_key flipped compared_pieces

(* Tourne l'echiquier pour avoir le roi dans le triangle en bas a gauche de l'echiquier *)
let rec normalize_board (board : Board.chessboard)
    (pieces : Board.colored_chesspiece list) =
  if is_normalized board pieces then board
  else
    let n = Bitboard.get_lsb board.wking in
    match Bitboard.coord_of_index n with
    (* Le roi est bien place mais l'echiquier n'est pas normalise, donc la piece suivante n'est pas bien placee *)
    | i, j when i < 4 && j < 4 && i <= j -> transform_board flip_diagonal board
    (* Triangle gauche en bas a gauche *)
    | i, j when i < 4 && j < 4 && i > j ->
        normalize_board (transform_board flip_diagonal board) pieces
    (* Triangle gauche en bas a droite *)
    | i, j when i < 4 && j >= 4 && i <= 7 - j ->
        normalize_board (transform_board flip_diagonal board) pieces
    (* Triangle droit en bas a droite *)
    | i, j when i < 4 && j >= 4 && i > 7 - j ->
        normalize_board (transform_board rotate_clockwise board) pieces
    (* Triangle gauche en haut a gauche *)
    | i, j when i >= 4 && j < 4 && i <= 7 - j ->
        normalize_board (transform_board rotate_counterclockwise board) pieces
    (* Triangle droit en haut a gauche *)
    | i, j when i >= 4 && j < 4 && i > 7 - j ->
        normalize_board (transform_board flip_horizontal board) pieces
    (* Triangle gauche en haut a droite *)
    | i, j when i >= 4 && j >= 4 && i > j ->
        normalize_board (transform_board rotate_counterclockwise board) pieces
    (* Triangle droit en haut a droite *)
    | i, j when i >= 4 && j >= 4 && i <= j ->
        normalize_board (transform_board flip_antidiagonal board) pieces
    | _ -> failwith "Cas impossible"
