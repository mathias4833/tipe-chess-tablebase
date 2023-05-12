open Int64;;


(* Genere le masque associe aux coordonnees i j *)
let generate_mask i j =
  let n = 8 * i + j in
  let mask = ref 0L in
  (* Remplace la ligne et la colonne ou se trouve la piece par des 1 *)
  mask := logor (shift_left Board.line (8*i)) (shift_left Board.column j);

  (* Supprime la case ou se trouve la piece *)
  mask := logand !mask (lognot (shift_left 1L n)); 

  (* Supprime les bords si la case n'est pas sur un bord, 
    reduit le nombre de positions possibles *)
  if i != 0 then
    mask := logand !mask (lognot Board.line);
  if i != 7 then
    mask := logand !mask (lognot (shift_left Board.line (8*7)));
  if j != 0 then
    mask := logand !mask (lognot Board.column);
  if j != 7 then
    mask := logand !mask (lognot (shift_left Board.column 7));
  
  !mask
;;


let generate_blockers_aux l1 l2 c1 c2 =
  let blockers = ref 0L in
  for n = 1 to l1 - 1 do
    blockers := logor !blockers (Utils.create_board l1 n);
  done;
  for n = l2 + 1 to 6 do
    blockers := logor !blockers (Utils.create_board l2 n)
  done;
  for n = 1 to c1 - 1 do
    blockers := logor !blockers (Utils.create_board n c1)
  done;
  for n = c2 + 1 to 6 do
    blockers := logor !blockers (Utils.create_board n c2)
  done;
  !blockers
;;

(* Genere l'ensemble des positions avec des bloqueurs, pour une case donnee *)
let generate_blockers i j =
  let mask = generate_mask i j in

  let start_line = if i = 0 then -1 else 0 in
  let stop_line = if i = 7 then 8 else 7 in
  let start_column = if j = 0 then -1 else 0 in
  let stop_column = if j = 7 then 8 else 7 in

  for l1 = start_line to (j-1) do
    for l2 = stop_line downto (j+1) do
      for c1 = start_column to (i-1) do
        for c2 = stop_column downto (i+1) do
          let nearest_blockers = List.fold_left logor [
            Utils.create_board l1 j;
            Utils.create_board l2 j;
            Utils.create_board i c1;
            Utils.create_board i c2] 0L in

          (* Liste contenant les bloqueurs possibles *)
          let blockers =
            let rec aux l acc = 
            in generate_combinations (generate_blockers_aux l1 l2 c1 c2);;
          
          
          
        done;
      done;
    done;
  done;
    
    
    
;;
