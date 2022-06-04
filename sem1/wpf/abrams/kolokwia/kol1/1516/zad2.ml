type 'a tree = 
	| Node of 'a tree * 'a * 'a tree 
	| Null


let t = Node( 
			Node(
				Node(Null, 7, Null), 3, Null), 1,
			Node(
				Node(Null, 4, Null), 2,
					Node(Null, 5,
						Node(Null, 6,
							Node(Null, 8, Null)
							)
						)
					
				)
			)

let srednie t =
	let rec pom t g =
		match t with
		| Null -> (0, [])
		| Node (l, x, r) ->
			let (gl, ll) = pom l (g + 1)
			and (gr, lr) = pom r (g + 1)
			in
				if g = max gl gr then
					(max gl gr + 1, x::(ll@lr))
				else
					(max gl gr + 1, ll@lr)

	in pom t 0;;
