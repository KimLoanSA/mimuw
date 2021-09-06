type tr = Node of tr * tr | Leaf

let listaliczNode d0 =
	let rec spacer d l =
		match d with 
		|Leaf -> l
		|Node (dl, dp) -> 
			let (h, t) =
				match l with
				|[]-> (0,[])
				|h::t -> (h,t)
			in 
				(h + 1)::(spacer dl (spacer dp t))
	in
		spacer d0 []
;;