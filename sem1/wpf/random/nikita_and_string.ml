open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)
let read_string () = bscanf Scanning.stdib " %s " (fun x -> x)

let () = 
	let s = read_string () 
	and wyn = ref 0 in
	let n = String.length s in

	let a = Array.make (n + 1) 0 
	and b = Array.make (n + 1) 0 in

	for i = 1 to n do 
		a.(i) <- a.(i - 1);
		b.(i) <- b.(i - 1);

		if s.[i - 1] = 'a' then 
			a.(i) <- a.(i) + 1
		else 
			b.(i) <- b.(i) + 1
	done;

	for i = 0 to n do 
		for j = i + 1 to n do 
			let bb = b.(i) + b.(n) - b.(j)
			and aa = a.(j) - a.(i) in

			wyn := max !wyn (n - bb - aa);
		done;
	done;

	for i = 1 to n do 
		let aa = a.(n) - a.(i) 
		and bb = b.(i) in
		wyn := max !wyn (n - aa - bb);
	done;

	printf "%d\n" !wyn;;