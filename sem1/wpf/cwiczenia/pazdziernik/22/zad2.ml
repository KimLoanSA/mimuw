type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;

let rec lustro t = 
	match t with
	| Leaf -> Leaf
	| Node (l, w, p) -> Node (lustro p, w, lustro l)
;;