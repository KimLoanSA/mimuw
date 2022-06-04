open Printf

let segment tab =
	let n = Array.length tab 
	and pop1 = ref 1 
	and pop2 = ref 1 
	and wyn = ref 0 in

	let stos = Stack.create () in
	let fib = Hashtbl.create 10000 in

	Hashtbl.add fib 1 1;

	for i = 3 to n + 3 do
		let akt = !pop1 + !pop2 in

		Hashtbl.add fib akt i;

		pop2 := !pop1;
		pop1 := akt;
	done;

	for i = 0 to n - 1 do 
		if Stack.is_empty stos then
			Stack.push tab.(i) stos
		else if Hashtbl.mem fib tab.(i) = false then
			Stack.push tab.(i) stos 
		else begin
			let pop = ref (Stack.pop stos) in
			let akt = ref 0 in

			if Hashtbl.mem fib !pop = false then begin
				Stack.push !pop stos;
				Stack.push tab.(i) stos;
			end else begin
				akt := tab.(i);

				while (Stack.is_empty stos = false) && Hashtbl.mem fib (!pop + !akt) do
					akt := !pop + !akt;
					pop := Stack.pop stos;
				done;
				
				Stack.push !pop stos;
				Stack.push !akt stos;
			end;
		end;
	done;

	while Stack.is_empty stos = false do 
		let akt = Stack.pop stos in

		if Hashtbl.mem fib akt then
			let ind = Hashtbl.find fib akt in
			wyn := max !wyn ind;
	done;

	!wyn;