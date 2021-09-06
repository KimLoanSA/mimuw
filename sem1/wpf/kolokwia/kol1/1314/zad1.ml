type 'a tree = 
	| Node of 'a tree * 'a * 'a tree
	| Null
;;

let rec fold_tree f a t =
	match t with
	| Null -> a
	| Node (l, x, r) -> f x (fold_tree f a l) (fold_tree f a r)
;;

let levels t =
	let f x lf rf =
		fun acc ->
			match acc with
			| [] -> [x]::(lf (rf []))
			| h::t -> (x::h)::(lf (rf t))

	in 
		fold_tree f (fun x -> x) t []
;;


let f (x : 'a) (left : 'a tree -> 'a list list -> 'a list list) (right : 'a tree -> 'a list list -> 'a list list)  t acc =
  match t with
  | Null -> acc
  | Node (l, x, r) ->
    let lista = (right r (tail acc)) in
      (x :: (head acc)) :: (left l lista)


let levels t = (fold_tree (f) (fun t acc -> acc) (t)) t []