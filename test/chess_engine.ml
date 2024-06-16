open Utils
open Moves
open Endgame

let fail fmt = Printf.ksprintf failwith fmt
let assert_true cond msg = if not cond then fail "%s" msg

let assert_equal_int64 actual expected msg =
  if actual <> expected then
    fail "%s: expected %Ld, got %Ld" msg expected actual

let assert_equal_bool actual expected msg =
  if actual <> expected then fail "%s: expected %b, got %b" msg expected actual

let contains_move move moves = List.exists (( = ) move) moves

let empty_board color =
  { (Board.empty_board color) with wking = 0x10L; bking = 0x1000000000000000L }

let test_bitboard_handles_high_bit () =
  let b = Bitboard.set_nth 0L 63 in
  assert_equal_int64 (Bitboard.get_nth b 63) 1L "Le bit 63 doit être lisible";
  assert_true (Bitboard.get_lsb b = 63) "Le bit de poids faible doit être 63";
  assert_true (Bitboard.count_ones b = 1) "count_ones doit gérer le bit 63"

let test_black_castling_generation_and_execution () =
  let board =
    {
      (empty_board Board.Black) with
      brooks = Int64.logor (Bitboard.set_nth 0L 56) (Bitboard.set_nth 0L 63);
      bcastle = true;
    }
  in
  let moves = Castling.generate_moves board in
  assert_true
    (contains_move Board.ShortCastling moves)
    "Le petit roque noir doit être généré";
  assert_true
    (contains_move Board.LongCastling moves)
    "Le grand roque noir doit être généré";

  let short_board = Move.play_move board Board.ShortCastling in
  assert_equal_int64 short_board.bking (Bitboard.set_nth 0L 62)
    "Le petit roque noir doit déplacer le roi";
  assert_equal_int64 short_board.brooks
    (Int64.logor (Bitboard.set_nth 0L 56) (Bitboard.set_nth 0L 61))
    "Le petit roque noir doit déplacer la tour";
  assert_equal_int64 short_board.wrooks 0L
    "Le petit roque noir ne doit pas modifier les tours blanches";

  let long_board = Move.play_move board Board.LongCastling in
  assert_equal_int64 long_board.bking (Bitboard.set_nth 0L 58)
    "Le grand roque noir doit déplacer le roi";
  assert_equal_int64 long_board.brooks
    (Int64.logor (Bitboard.set_nth 0L 59) (Bitboard.set_nth 0L 63))
    "Le grand roque noir doit déplacer la tour";
  assert_equal_int64 long_board.wrooks 0L
    "Le grand roque noir ne doit pas modifier les tours blanches"

let test_castling_rights_removed_when_rook_moves_or_is_captured () =
  let white_rook_board =
    {
      (empty_board Board.White) with
      wrooks = Bitboard.set_nth 0L 7;
      wcastle = true;
    }
  in
  let moved_white_rook =
    Move.play_move white_rook_board (Board.Chessmove (Board.R, 7, 15, None))
  in
  assert_equal_bool moved_white_rook.wcastle false
    "Le déplacement d'une tour blanche doit retirer le droit de roque";

  let black_rook_board =
    {
      (empty_board Board.Black) with
      brooks = Bitboard.set_nth 0L 63;
      bcastle = true;
    }
  in
  let moved_black_rook =
    Move.play_move black_rook_board (Board.Chessmove (Board.R, 63, 55, None))
  in
  assert_equal_bool moved_black_rook.bcastle false
    "Le déplacement d'une tour noire doit retirer le droit de roque";

  let captured_white_rook_board =
    {
      (empty_board Board.Black) with
      wrooks = Bitboard.set_nth 0L 0;
      bqueen = Bitboard.set_nth 0L 8;
      wcastle = true;
    }
  in
  let after_capture =
    Move.play_move captured_white_rook_board
      (Board.Chessmove (Board.Q, 8, 0, Some (Board.R, Board.White)))
  in
  assert_equal_bool after_capture.wcastle false
    "La capture d'une tour blanche doit retirer le droit de roque"

let test_play_unmove_restores_captured_piece_without_losing_others () =
  let board =
    {
      (empty_board Board.White) with
      wrooks = Bitboard.set_nth 0L 0;
      brooks = Int64.logor (Bitboard.set_nth 0L 56) (Bitboard.set_nth 0L 63);
    }
  in
  let after_capture =
    Move.play_move board
      (Board.Chessmove (Board.R, 0, 56, Some (Board.R, Board.Black)))
  in
  let restored =
    Move.play_unmove after_capture
      (Board.Chessmove (Board.R, 0, 56, Some (Board.R, Board.Black)))
  in
  assert_equal_int64 restored.brooks
    (Int64.logor (Bitboard.set_nth 0L 56) (Bitboard.set_nth 0L 63))
    "play_unmove doit restaurer la tour capturée sans supprimer les autres"

let test_serializer_handles_identical_pieces () =
  let pieces = [ (Board.R, Board.White); (Board.R, Board.White) ] in
  let board =
    {
      (Board.empty_board Board.Black) with
      wking = Bitboard.set_nth 0L 2;
      bking = Bitboard.set_nth 0L 63;
      wrooks =
        Int64.logor (Bitboard.set_nth 0L 8) (Bitboard.set_nth 0L 17);
    }
  in
  let normalized = Transformations.normalize_board board pieces in
  let index = Serializer.board_to_number normalized pieces in
  let restored = Serializer.number_to_board index pieces in
  assert_true (restored = normalized)
    "La sérialisation doit conserver deux pièces identiques";

  let captured =
    { normalized with wrooks = Bitboard.set_nth 0L 8 }
  in
  let captured_index = Serializer.board_to_number captured pieces in
  let restored_captured = Serializer.number_to_board captured_index pieces in
  assert_true (restored_captured = captured)
    "La sérialisation doit conserver l'absence d'une pièce identique";

  let other =
    {
      normalized with
      wrooks =
        Int64.logor (Bitboard.set_nth 0L 8) (Bitboard.set_nth 0L 18);
    }
  in
  assert_true
    (Serializer.board_to_number other pieces <> index)
    "Deux positions différentes doivent avoir des indices différents"

let test_duplicate_piece_retro_capture () =
  let pieces = [ (Board.R, Board.White); (Board.R, Board.White) ] in
  let board =
    {
      (Board.empty_board Board.Black) with
      wking = Bitboard.set_nth 0L 0;
      bking = Bitboard.set_nth 0L 63;
      wrooks = Bitboard.set_nth 0L 8;
    }
  in
  let unmoves =
    Move.add_unmoves_to_list Board.K 63 [] board pieces
      (Bitboard.set_nth 0L 62)
  in
  let captures =
    List.filter
      (function
        | Board.Chessmove (_, _, _, Some (Board.R, Board.White)) -> true
        | _ -> false)
      unmoves
  in
  assert_true (List.length captures = 1)
    "La génération rétrograde doit restaurer une tour manquante"

let test_normalization_handles_identical_pieces () =
  let pieces = [ (Board.R, Board.White); (Board.R, Board.White) ] in
  let board =
    {
      (Board.empty_board Board.Black) with
      wking = Bitboard.set_nth 0L 9;
      bking = Bitboard.set_nth 0L 27;
      wrooks =
        Int64.logor (Bitboard.set_nth 0L 6) (Bitboard.set_nth 0L 40);
    }
  in
  let flipped = Transformations.transform_board Transformations.flip_diagonal board in
  assert_true
    (Transformations.normalize_board board pieces
    = Transformations.normalize_board flipped pieces)
    "Deux positions symétriques doivent avoir la même normalisation"

let () =
  test_bitboard_handles_high_bit ();
  test_black_castling_generation_and_execution ();
  test_castling_rights_removed_when_rook_moves_or_is_captured ();
  test_play_unmove_restores_captured_piece_without_losing_others ();
  test_serializer_handles_identical_pieces ();
  test_duplicate_piece_retro_capture ();
  test_normalization_handles_identical_pieces ()
