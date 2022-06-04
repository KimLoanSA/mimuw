let appendr li1 li2 =
	list.fold_right (fun h a -> h::a) li1 li2
;;

let appendl li1 li2 =
	let reverse xs = 
		list.fold_left (fun y ys -> y::ys) [] xs
	in 
		reverse (List.fold_left (fun y ys -> y::ys) (reverse li1 ) li2 )
;; 