open List;;

type 'a tree = Node of 'a * 'a tree list

let rec fold_tree f (Node(x, l)) =
	f x (map (fold_tree f) l);;

let wezly t =
	let f _ l = 
		fold_left (+) 1 l
	in
		fold_tree f t;;
