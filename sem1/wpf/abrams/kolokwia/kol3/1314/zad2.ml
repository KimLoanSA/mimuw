open Printf 

let myszy k wej =
	let n = Array.length wej in

	let dp = Array.make_matrix (n + 1) (n + 1) 0 
	and prefix = Array.make (n + 1) 0 
	and tab = Array.make (n + 1) 0 in

	for i = 0 to n - 1 do
		tab.(i + 1) <- wej.(i);
	done;

	for i = 1 to n do 
		prefix.(i) <- prefix.(i - 1) + tab.(i);
	done;

	for i = 1 to n do
		for l = 1 to k do
			for j = 1 to i do
				dp.(i).(l) <- max (max dp.(i).(l) dp.(i - 1).(l)) (dp.(j - 1).(l - 1) + (max 0 (prefix.(i) - prefix.(j - 1) - ((j - i) * (j - i)))));
				printf "%d " dp.(i).(l);
			done;
			printf "\n";
		done;
		printf "\n";
	done;


	dp.(n).(k)
