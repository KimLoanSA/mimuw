type 'a elem = {v : 'a; mutable next : 'a lista; mutable prev : 'a lista}
and 'a lista = 'a elem option;;

let daj x =
	match x with
	| Some x -> ref x
(*
let codrugi l =
	let wyn = ref None 
	and licznik = ref 1
	and pop1 = ref l
	and akt = (!(daj !l)).next in
	while !l <> None do 
		let akt = daj !l in
		if !licznik mod 2 = 0 then
			if !licznik = 2 then
			begin
				(!akt).prev <- !pop1;
				pop2 = ref akt;
			end
			else
			begin
				l.prev = ref wyn;
				wyn.next = ref l;
			end
		else
		begin
			pop1.next = ref l;
			l.prev = ref pop1;
			pop1 = ref l;
		end

		l := (!l).next;
		licznik := !licznik + 1;
	done;
;;
*)

let get_element e = 
  match e with
  |Some x -> ref x;;

let codrugi l =
  	if l = None then 
  		None 
  	else
	  	let wyn = ref None 
	 	and pom = ref l 
	 	and it = ref l 
	 	and mem = ref None 
	  	and ind = ref 1 in 
	  	while !it <> None do
	    	let lit = get_element !it in
	    	(!lit).prev <- None;
	    	if !ind mod 2 = 1 then 
	    	begin
	    		if(!ind == 1) then () 
	  			else begin
	        	let lpom = get_element !pom in
	        	(!lit).prev <- !pom;
	        	(!lpom).next <- !it;
	        	pom := Some !lit;
	   			end;
	    	end else 
	    	begin
	      	if !wyn == None then 
	      	begin 
	      		wyn := Some (!lit); 
	      		mem := !wyn 
	      	end else 
	      	begin
	        	let lwyn = get_element !wyn in
	        	(!lit).prev <- !wyn;
	        	(!lwyn).next <- !it;
	        	wyn := Some !lit;
	        (*assert(Some !lwyn = !wyn);*)
	      	end;
	    end;
	    it := (!lit).next;
	    incr ind;
	  done;
	  if !wyn <> None then let lwyn = get_element !wyn in (!lwyn).next <- None; else ();
	  if !pom <> None then let lpom = get_element !pom in (!lpom).next <- None; else ();
	  !mem;;








	  