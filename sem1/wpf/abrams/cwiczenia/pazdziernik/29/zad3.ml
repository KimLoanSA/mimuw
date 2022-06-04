let exists li p =
	list.fold_left (fun a h -> if a = true then a else p h) false li;;

let exists li p = 
	try 
		list.fold_left (fun a h -> if p h then raise Jest else false) false li
	with
		Jest -> true
;;