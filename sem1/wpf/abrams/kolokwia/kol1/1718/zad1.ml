open List;;

type 'a tree =
	| Node of 'a * 'a tree list


let rec pom l akt (rm, lm) =
	match l with
	| [] -> (rm, lm)
	| (rh, lh)::t ->
		if hd lh > akt && rh + 1 > rm then
			pom t akt (rh + 1, akt::lh)
		else
			pom t akt (rm, lm)
and 
	



		






;;

