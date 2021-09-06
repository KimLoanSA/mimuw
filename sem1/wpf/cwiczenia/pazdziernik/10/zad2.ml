let p f n =
	let rec pom a b s m d =
		if b = n + 1 then d
		else if s + f b > m then pom a ( b + 1 ) ( s + f b ) ( s + f b ) ( b - a + 1 )
		else if s + f b < 0 then pom ( b + 1 ) ( b + 1 ) 0 m d
		else pom a ( b + 1 ) ( s + f b ) m d

	in 
		pom 1 1 0 ( f 1 ) 1
;;

let p2 f n =
