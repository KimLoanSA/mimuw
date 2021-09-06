open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () =
	let n = read_int ()
	and b = read_int () in

	let parzyste = ref 0
	and nieparzyste = ref 0 
	and wyn = ref 0
	and suma = ref 0 in

	let koszt = Array.make (n + 1) 2137 
	and tab = Array.make (n + 1) 1822 in

	for i = 0 to n - 1 do 
		tab.(i) <- read_int ()
	done;

	for i = 0 to n - 2 do
		if tab.(i) mod 2 = 0 then
			incr parzyste
		else 
			incr nieparzyste;

		if !nieparzyste = !parzyste then
			koszt.(i) <- abs (tab.(i) - tab.(i + 1))
	done;

	Array.sort compare koszt;

	for i = 0 to n - 2 do 
		if !suma + koszt.(i) <= b then begin
			suma := !suma + koszt.(i);
			incr wyn
		end;
	done;

	printf "%d\n" !wyn
