type 'a option = None | Some of 'a
type 'a element = {v: 'a; mutable next: 'a lista}
and 'a lista = 'a element option


let przeplot l1 l2 =
	let rec pom l e =
		let nast = e.next in
		begin
			e.next <- l;
			match l with
			| None -> e.next <- nast;
			| Some e2 -> pom nast e2
		end in
 	match l1 with
 	| None -> l2
 	| Some e ->
 		begin
 			pom l2 e;
 			l1
 		end
