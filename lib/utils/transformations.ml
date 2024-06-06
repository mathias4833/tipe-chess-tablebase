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
  let i, j = get_coord board (Board.K, Board.White) in
  if j > 3 || i > 3 then false
  else if i = j then aux ((Board.K, Board.Black) :: pieces)
  else i < j

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
