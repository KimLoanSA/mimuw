open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () =
	let n = read_int () 
	and m = read_int () 
	and wyn = ref 0 in

	let g = Array.make (n + 1) [] 
	and dp = Array.make (n + 1) 0 in

	for i = 1 to m do 
		let a = read_int ()
		and b = read_int () in

		g.(a) <- b::g.(a);
		g.(b) <- a::g.(b);
	done;

	for i = 2 to n do
		dp.(i) <- (List.fold_left (fun acc a -> if a < i then max acc dp.(a) else acc) (-1) g.(i)) + 1;
		wyn := max !wyn dp.(i);
	done;

	printf "%d\n" !wyn
;;