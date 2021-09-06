open List;;

let przestaw t =
	let ind = ref 0
	and ind_pom = ref 0
	and pom = Array.make (Array.length t) 0 
	and wyn = Array.make (Array.length t) 0 in
		for i = 0 to Array.length t - 1 do 
			if t.(i) < 0 then
				begin
					pom.(!ind_pom) <- -t.(i);
					ind_pom := !ind_pom + 1;
				end
			else
				begin
					while !ind_pom > 0 && pom.(!ind_pom - 1) < t.(i) do
						t.(!ind) <- pom.(!ind_pom - 1);
						ind := !ind + 1;
						ind_pom := !ind_pom - 1;
					done;
					t.(!ind) <- t.(i);
					ind := !ind + 1;
				end;
		done;
		t;;