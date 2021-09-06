open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () =
	let n = read_int () in 
	let g = Array.make (n + 1) []
	and odwiedzone = Array.make (n + 1) false
	and kolor = Array.make (n + 1) 0 in
	let wyn = ref 0 in
	let rec dfs v kol =
		if odwiedzone.(v) = false then
		begin
			odwiedzone.(v) <- true;
			if kolor.(v) <> kol then
				wyn := !wyn + 1;
			List.iter (fun w -> dfs w kolor.(v)) g.(v)
		end in
	for i = 2 to n do 
		let a = read_int () in
		g.(a) <- i::g.(a);
		g.(i) <- a::g.(i)
	done;

	for i = 1 to n do
		kolor.(i) <- read_int ();
	done;

	dfs 1 0;

	printf "%d\n" !wyn;;