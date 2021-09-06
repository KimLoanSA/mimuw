open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () = 
	let n = read_int ()
	and m = read_int () in

	let tab = Array.make n 0
	and uzyte = Array.make 100007 false 
	and dp = Array.make (n + 1) 0 in

	for i = 0 to n - 1 do
		tab.(i) <- read_int ()
	done;

	for i = n - 1 downto 0 do 
		if uzyte.(tab.(i)) = false then
			dp.(i) <- dp.(i + 1) + 1
		else
			dp.(i) <-  dp.(i + 1);

		uzyte.(tab.(i)) <- true;
	done;

	for i = 1 to m do
		let a = read_int () in
		printf "%d\n" dp.(a - 1);
	done;