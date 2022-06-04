type 'a tree = Node of 'a tree * 'a * 'a tree | Leaf;;

let rec licz t = 
	match t with 
	| Leaf -> (0, 0)
	| Node(l, _, p) -> 
		let (lr, lg) = licz l
		and
			(pr, pg) = licz p
		in
			(lr + pr + 1, max lg pg + 1)
;;