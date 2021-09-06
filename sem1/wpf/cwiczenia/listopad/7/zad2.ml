type 'a bin_tree =  Node of 'a bin_tree * 'a * 'a bin_tree | Null

let rec fold_bin_tree f a t = 
	match f with
	|Null -> a
	|Node (l, x, r) -> f x (fold_bin_tree f a l) (fold_bin_tree f a r)

(* liczba wierzcholkow node w drzewie *)

let rozmiar d =
	fold_bin_tree (fun w rl rr -> rl + rr + 1) 0 d
;;
