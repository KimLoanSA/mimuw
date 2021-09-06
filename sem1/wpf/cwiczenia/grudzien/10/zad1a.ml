type 'a element = {v: 'a; mutable next: 'a lista}
and 'a lista = 'a element option;;

let elem x =
	match x with
	| Some x -> x

let przeplot l1 l2 =
	let it1 = ref l1 
	and it2 = ref l2 
	and pop = ref None in
	begin
		while !it1 <> None && !it2 <> None do 
			let xit1 = elem !it1 in
			if !pop == None then
				xit1.next <- !it2
			else
			begin
				let xpop = elem !pop in
				begin 
				xpop.next <- !it1;
				xit1.next <- !it2;
				end;
			end;
			pop := !it2;
		done;
		let xpop = elem !pop in
		if !it1 == None then
			xpop.next <- !it2
		else
			xpop.next <- !it1
	end;

let listuj l = 
 	let f x a =
 		let wyn = Some {v = x; next = a} in
 		wyn
 	in
 	List.fold_right f l None
 ;;