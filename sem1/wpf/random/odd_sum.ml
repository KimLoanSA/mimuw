open Printf
open Scanf
let read_int _ = bscanf Scanning.stdib " %d " (fun x -> x)

let () = 
	let n = read_int ()
	and suma = ref 0
	and mini = ref 10007 
	and maxi = ref (-10007) in

	for i = 1 to n do 
		let a = read_int () in
		if a > 0 then begin
			suma := !suma + a;
			if a mod 2 = 1 then
				mini := min !mini a
		end else if abs (a mod 2) = 1 then
			maxi := max !maxi a;
	done;

	if !suma mod 2 = 1 then
		printf "%d\n" !suma
	else
		printf "%d\n" (max (!suma - !mini) (!suma + !maxi))

