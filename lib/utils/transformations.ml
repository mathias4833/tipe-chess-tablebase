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

(* Effectue un symetrie verticale *)
let flip (i, j) = (i, 7 - j)

(* Verifie si l'echiquier est norm*)
let is_normalized (board : Board.chessboard) =
  let i, j = Bitboard.coord_of_index (Bitboard.get_lsb board.wking) in
  j < 4 && i <= j

(* Tourne l'echiquier pour avoir le roi dans le triangle en bas a gauche de l'echiquier *)
let rec normalize_board (board : Board.chessboard) (move : Board.chessmove) =
  let transform_aux f b m = (transform_board f b, transform_move f m) in

  let n = Bitboard.get_lsb board.wking in
  match Bitboard.coord_of_index n with
  | i, j when j < 4 && i <= j -> (board, move)
  | i, j when j >= 4 && i <= 7 - j -> transform_aux flip board move
  | i, j when i < 4 && i > j ->
      transform_aux (flip << rotate_counterclockwise) board move
  | i, j when i >= 4 && j <= 7 - i ->
      transform_aux rotate_counterclockwise board move
  | _ ->
      let b, m =
        transform_aux
          (rotate_counterclockwise << rotate_counterclockwise)
          board move
      in
      normalize_board b m
