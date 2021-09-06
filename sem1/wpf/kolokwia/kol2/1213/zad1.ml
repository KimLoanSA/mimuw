type elem = {x : int; mutable prev : elem list};;
type lista = elem list;;

let element x =
	match x with
	| Some x -> ref x

let ost l =
	let it = ref l in
	while !it <> None do 
		let it1 = element !it in
		it := (!it1).next;
	done;
	!it
;;
(*let ustaw l =
	let n = List.length l in
	n;;
*)