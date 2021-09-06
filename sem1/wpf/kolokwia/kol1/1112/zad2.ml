type 'a tree =
	| Node of 'a * 'a tree * 'a tree
	| Leaf

let t = Node(5, 
		Node(4, Leaf, Node(2, Leaf, Leaf)),
		Node(6, 
			Node(1, Leaf, Leaf),
			Node(3, Leaf, Leaf)
		)
	)

let d = Node(4, Leaf, Node(2, Leaf, Leaf))

let skosokosc t = 
	let rec pom t =
		match t with 
		| Leaf -> (Leaf, 0)
		| Node (w, l, p) ->
			let (dL, sL) = pom l
			and (dP, sP) = pom p
			in
				if (max (sP * 2) (sL * 2 + 1)) < (max (sL * 2) (sP * 2 + 1)) then
					(Node(w, dP, dL), max (sP * 2) (sL * 2 + 1))
				else
					(Node(w, dL, dP), max (sL * 2) (sP * 2 + 1))
	in			
		pom t
;;
