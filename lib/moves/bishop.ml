open Int64;;
open Utils;;

let generate_mask  i j =
	let pos = 1L in
	let mask = ref 0L in
	for k=1 to 7 do 
		let a = i+k in
		let b = i-k in 
		let c = j+k in
		let d = j-k in
		if 0<a && 7>a && 0<c && 7>c then mask:= logor !mask (shift_left pos (a*8+c));
		if 0<a && 7>a && 0<d && 7>d then mask:= logor !mask (shift_left pos (a*8+d));
		if 0<b && 7>b && 0<c && 7>c then mask:= logor !mask (shift_left pos (b*8+c));
		if 0<b && 7>b && 0<d && 7>d then mask:= logor !mask (shift_left pos (b*8+d));
	done;
	!mask
	;;

let generate_blockers_from_nearest i j dhd dhg dbd dbg =
  let blockers = ref 0L in
  for n = dhg+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i+n) (j-n));
  done;
  for n = dhd+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i+n) (j+n))
  done;
  for n = dbd+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i-n) (j+n))
  done;
  for n = dbg+1 to 6 do
    blockers := logor !blockers (Bitboard.from_coordinate (i-n) (j-n))
  done;
  !blockers
;;

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)
let generate_blockers i j =
  let mask = generate_mask i j in 
  let blockers_list = ref [] in
  
  let stop_cg = if j = 0 then -1 else 1 in
  let stop_lb = if i = 0 then -1 else 1 in
  let stop_cd = if j = 7 then -1 else 1 in
  let stop_lh = if j = 7 then -1 else 1 in
  
  for dhg = 1 to (Int.min (7-i+stop_lh) (j+stop_cg)) do
    for dhd = 1 to (Int.min (7-i+stop_lh) (7-j+stop_cd)) do
      for dbg = 1 to (Int.min (i+stop_lb) (j+stop_cg)) do
        for dbd = 1 to (Int.min (i+stop_lb) (7-j+stop_cd)) do
          print_endline "hey";
          let nearest_blockers = List.fold_left logor 0L [
            Bitboard.from_coordinate (i+dhd) (j+dhd);
            Bitboard.from_coordinate (i+dhg) (j-dhg);
            Bitboard.from_coordinate (i-dbd) (j+dbd);
            Bitboard.from_coordinate (i-dbg) (j-dbg)] in
          let full_blockers = generate_blockers_from_nearest i j dhd dhg dbd dbg in
          let accessible_mask = logand mask (lognot (logor nearest_blockers full_blockers)) in

          (* Liste contenant les bloqueurs possibles *)
          let blockers =
            let rec aux l acc =
              match l with
              |[] -> acc
              |h::t -> aux t ((logor h nearest_blockers)::acc)
            in aux (Bitboard.generate_combinations full_blockers) []
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



