open Int64;;

(*
let checkmate chessboard =
  (*dit si un joueur est en position de mat*)
  (*liste des coups possibles*)
  (*si tous illégaux :
      -si le roi est en échec:
          checkmate
      -sinon : 
          pat        *)*)
                                
                                
                                
let nextcase pos =
  let pos2 = 1L in 
  let posf = shift_left pos2 (pos+8) in 
  posf;;




for i=8 to 55 do 
  print_endline (to_string (nextcase i))
  done;
  ;;



let generate_mask_bishop  i j =
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
  
  







