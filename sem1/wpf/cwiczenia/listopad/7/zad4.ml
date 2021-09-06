type 'a bin_tree =  Node of 'a bin_tree * 'a * 'a bin_tree | Null

let rec fold_bin_tree f a t = 
	match t with
	|Null -> a
	|Node (l, x, r) -> f x (fold_bin_tree f a l) (fold_bin_tree f a r)

(*let infix0 d =
	let spacer d = 
		fun l -> match d with
		|Null -> l
		|Node (dl, w, dr) -> 
			spacer dl (w::(spacer dr l))

	in
		spacer d []
*)

let infinx1 t =
	let f = 
		fun w fl fr -> fun aku ->
			fl (w::fr aku)
	in
		fold_bin_tree f (fun x -> x) t []
;;
