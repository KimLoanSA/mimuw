type 'a bin_tree =  Node of 'a bin_tree * 'a * 'a bin_tree | Null

let rec fold_bin_tree f a t = 
	match t with
	|Null -> a
	|Node (l, x, r) -> f x (fold_bin_tree f a l) (fold_bin_tree f a r)


let path d =
	let f = 
		fun w (dl, ll) (dr, lr) ->
			if dl > dr then
				(dl + 1, w::ll)
			else
				(dr + 1, w::lr)

	in 
		fold_bin_tree f (0, []) d
;;