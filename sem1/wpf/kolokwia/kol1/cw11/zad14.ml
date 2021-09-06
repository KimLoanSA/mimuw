open List;;

type 'a tree = Node of 'a * 'a tree list;;

let rec fold_tree f (Node (x, l)) = 
        f x (map (fold_tree f) l);;


let w5 = Node(5, [])
let w2 = Node(2, [w5])
let w7 = Node(7, [])
let w6 = Node(6, [w7])
let w8 = Node(8, [])
let w3 = Node(3, [w6;w8])
let w4 = Node(4, [])
let w1 = Node(1, [w2;w3;w4])

let f3 x l acc =
 	 x::( (fold_right(fun fa fb acc -> fa (fb acc)) l (fun x -> x)) acc)

let preorder t = (fold_tree f3 t) [];;

let postorder t =
	let f x l acc =
		x::((fold_left (fun fa fb acc -> fb (fa acc)) (fun x -> x) l) acc)

	in rev (fold_tree f t []);;