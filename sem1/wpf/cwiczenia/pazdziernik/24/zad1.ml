type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let lista t =
	let rec pom t li =
		match t with
		| Leaf -> li
		| Node (l, v, r) -> pom l (v::(pom r li))
	in
		pom t []
;;
