type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let srednica t =
	let rec pom t =
	