let zloz li = 
	list.fold_left (fun acc x -> fun x -> f (acc x)) (fun x -> x) li;;

let zloz2 li =
	fun x -> (list.fold_left (fun a f -> f a) x li;;