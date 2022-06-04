type 'a tree = 
	| Node of 'a tree * 'a * 'a tree
	| Leaf
;;

let rec fold_tree f a t =
	match t with
	| Leaf -> a
	| Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r) 
;;

let avl_shaped t =
	let f _ (bL,l) (bR, r) = 
		if abs(l - r) > 1 || bL = false || bR = false then
			(false, 0)
		else
			(true, max l r + 1)

	in fold_tree f (true, 0) t
;;