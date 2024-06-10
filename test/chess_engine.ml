open Utils
open Moves

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
  assert_equal_int64 (Bitboard.get_nth b 63) 1L "bit 63 should be readable";
  assert_true (Bitboard.get_lsb b = 63) "lsb should be 63";
  assert_true (Bitboard.count_ones b = 1) "count_ones should handle bit 63"

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
    "black short castling should be generated";
  assert_true
    (contains_move Board.LongCastling moves)
    "black long castling should be generated";

  let short_board = Move.play_move board Board.ShortCastling in
  assert_equal_int64 short_board.bking (Bitboard.set_nth 0L 62)
    "black short castling should move the king";
  assert_equal_int64 short_board.brooks
    (Int64.logor (Bitboard.set_nth 0L 56) (Bitboard.set_nth 0L 61))
    "black short castling should move the rook";
  assert_equal_int64 short_board.wrooks 0L
    "black short castling should not affect white rooks";

  let long_board = Move.play_move board Board.LongCastling in
  assert_equal_int64 long_board.bking (Bitboard.set_nth 0L 58)
    "black long castling should move the king";
  assert_equal_int64 long_board.brooks
    (Int64.logor (Bitboard.set_nth 0L 59) (Bitboard.set_nth 0L 63))
    "black long castling should move the rook";
  assert_equal_int64 long_board.wrooks 0L
    "black long castling should not affect white rooks"

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
    "white castling rights should be removed after rook move";

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
    "black castling rights should be removed after rook move";

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
    "white castling rights should be removed after rook capture"

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
    "play_unmove should restore captured rook without removing others"

let () =
  test_bitboard_handles_high_bit ();
  test_black_castling_generation_and_execution ();
  test_castling_rights_removed_when_rook_moves_or_is_captured ();
  test_play_unmove_restores_captured_piece_without_losing_others ()
