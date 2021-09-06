open List;;

type 'a tree = Node of 'a * 'a tree list

let rec fold_tree f (Node(x, l)) =
	f x (map (fold_tree f) l);;

let nieparzyste t =
	let f = fun _ lst ->
		let (w_n, w_w) = fold_left (fun (a_n, a_w) (x_n, x_w) -> (a_n + x_n, a_w + x_w)) (0,0) lst
		in	if w_w mod 2 = 0 then
				(w_n + 1, w_w + 1)
			else
				(w_n, w_w + 1)
	in fst (fold_tree f t);;
