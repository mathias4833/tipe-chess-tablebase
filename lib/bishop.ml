open Int64;;
                

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



let generate_access i j =
  let pos=1L in
  let accesslist = ref [] in
  let mask = ref (generate_mask i j) in
  for k=7 downto 1 do 
		let a = i+k in
		let b = i-k in 
		let c = j+k in
		let d = j-k in
    if 0<a && 8>a && 0<c && 8>c then ( mask:= logand !mask (lognot (shift_left pos (a*8+c)));
                                      accesslist:= !mask::!accesslist);
		if 0<a && 8>a && 0<d && 8>d then ( mask:= logand !mask (lognot (shift_left pos (a*8+d)));
                                      accesslist:= !mask::!accesslist);
		if 0<b && 8>b && 0<c && 8>c then ( mask:= logand !mask (lognot (shift_left pos (b*8+c)));
                                      accesslist:= !mask::!accesslist);
		if 0<b && 8>b && 0<d && 8>d then ( mask:= logand !mask (lognot (shift_left pos (b*8+d)));
                                      accesslist:= !mask::!accesslist);

  done;
  !accesslist
;;


let generate_blockers i j =
  let _mask = generate_mask i j in

  let start_line = if i = 0 then -1 else 0 in
  let stop_line = if i = 7 then 8 else 7 in
  let start_column = if j = 0 then -1 else 0 in
  let stop_column = if j = 7 then 8 else 7 in

  for l1 = start_line to (j-1) do
    for l2 = stop_line downto (j+1) do
      for c1 = start_column to (i-1) do
        for c2 = stop_column downto (i+1) do
          let _nearest_blockers = List.fold_left logor 0L [
            Utils.create_board l1 j;
            Utils.create_board l2 j;
            Utils.create_board i c1;
            Utils.create_board i c2] in
            ()
          
        done;
      done;
    done;
  done;
    
    
    
;;








