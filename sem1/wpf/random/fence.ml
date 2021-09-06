open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () = 
	let n = read_int ()
	and m = read_int () 
	and wyn = ref 100000000 
	and indeks = ref 1 in

	let tab = Array.init n read_int 
	and pref = Array.make (n + 1) 0 in

	for i = 1 to n do
		pref.(i) <- pref.(i - 1) + tab.(i - 1);
	done;

	for i = 1 to n - m + 1 do 
		let roznica = pref.(i + m - 1) - pref.(i - 1) in
		if roznica < !wyn then begin
			wyn := roznica;
			indeks := i;
		end;
	done;

	printf "%d\n" !indeks;;