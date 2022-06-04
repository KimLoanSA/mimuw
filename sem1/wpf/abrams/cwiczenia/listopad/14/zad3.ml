open List;;

type 'a tree = Node of 'a * 'a tree list

let rec fold_tree f (Node(x, l)) =
	f x (map (fold_tree f) l);;

let zrwn t =
	let f = fun _ l ->
		match l with
		| [] -> (0, 0)
		| h::t -> 
			let (mini, maxi) = fold_left (fun (mini_a, maxi_a) (mini_e, maxi_e) -> (min mini_a mini_e, max maxi_a maxi_e)) h t
			in
				(mini + 1, maxi + 1)

	in
		let (mini, maxi) = fold_tree f t
		in mini = maxi
;;


exception Nie;;

let zrwm2 t =
	let f = fun aku x ->
		if aku = 0 then 
			x + 1
		else if aku = x + 1 then
			aku
		else
			raise Nie

	in try
		(match (fold_tree (fun x li -> fold_left f 0 li) t)
			with _-> true)

		with Nie -> false