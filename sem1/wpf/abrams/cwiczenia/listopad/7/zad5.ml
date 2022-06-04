type 'a bin_tree =  Node of 'a bin_tree * 'a * 'a bin_tree | Null

let rec fold_bin_tree f a t = 
	match t with
	|Null -> a
	|Node (l, x, r) -> f x (fold_bin_tree f a l) (fold_bin_tree f a r)

let przys d =
	let f = 
		fun w fl fr -> fun max ->
		if w >= max then
			fl w + fr w + 1
		else 
			fl max + fr max
	in
		fold_bin_tree f (fun x -> 0) d 0
;;
		