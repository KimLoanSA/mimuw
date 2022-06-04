open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () =
	let n = read_int ()
	and m = read_int () 
	and wyn = ref 0 in

	let tab = Array.make (n + 1) 100000 in

	for i = 0 to n - 1 do 
		tab.(i) <- read_int ()
	done;

	for i = 1 to n - 1 do 
		if tab.(i) + tab.(i - 1) < m then begin
			wyn := !wyn + abs(m - (tab.(i) + tab.(i - 1)));
			tab.(i) <- m - tab.(i - 1);
		end;
	done;

	printf "%d\n" !wyn;

	for i = 0 to n - 1 do 
		printf "%d " tab.(i)
	done;