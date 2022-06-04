open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () = 
	let n = read_int ()
	and m = read_int () 
	and wyn = ref 0L in

	let g = Array.make (n + 1) []
	and dlugosc = Array.make (n + 1) 1L in

	for i = 1 to m do 
		let a = read_int ()
		and b = read_int () in

		g.(a) <- b::g.(a);
		g.(b) <- a::g.(b);
	done;

	for i = 1 to n do 
		let s = Int64.of_int (List.length g.(i)) in

		List.iter (fun x -> if x < i then dlugosc.(i) <- max dlugosc.(i) (Int64.add dlugosc.(x) 1L);) g.(i);

		wyn := max !wyn (Int64.mul dlugosc.(i) s)
	done;

	printf "%Ld\n" !wyn;;