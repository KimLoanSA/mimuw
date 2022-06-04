(* Implementacja drzew find-union. *)
module type FIND_UNION = sig
  (* Typ klas elementów typu 'a. *)
  type 'a set

  (* Tworzy nową klasę złożoną tylko z danego elementu. *)
  val make_set : 'a -> 'a set

  (* Znajduje reprezentanta danej klasy. *)
  val find : 'a set -> 'a

  (* Sprawdza, czy dwa elementy są równoważne. *)
  val equivalent : 'a set -> 'a set -> bool 

  (* Scala dwie dane (rozłączne) klasy. *)
  val union : 'a set -> 'a set -> unit

  (* Lista elementów klasy. *)
  val elements : 'a set -> 'a list

  (* Liczba wszystkich klas. *)
  val n_of_sets : unit-> int
end;;


module Find_Union : FIND_UNION = struct
  (* Typ klas elementów typu 'a. *)
  type 'a set = { 
    elem         : 'a;           (* Element klasy. *)
    up           : 'a set ref;   (* Przodek w drzewie find-union. *)
    mutable rank : int;          (* Ranga w drzewie find-union. *)
    mutable next : 'a set list   (* Lista potomków w pewnym drzewie rozpinającym klasę. *)
  }

  (* Licznik klas. *)
  let sets_counter = ref 0

  (* Liczba wszystkich klas. *)
  let n_of_sets () = !sets_counter

  (* Tworzy nową klasę złożoną tylko z danego elementu. *)
  let make_set x = 
    let rec v = { elem = x; up = ref v; rank = 0; next = [] }
    in begin
      sets_counter := !sets_counter + 1;
      v
    end

  (* Znajduje korzeń drzewa, kompresując ścieżkę. *)
  let rec go_up s = 
    if s == !(s.up) then s 
      else begin
	s.up := go_up !(s.up);
	!(s.up)
      end

  (* Znajduje reprezentanta danej klasy. *)
  let find s = 
    (go_up s).elem
	
  (* Sprawdza, czy dwa elementy są równoważne. *)
  let equivalent s1 s2 =
    go_up s1 == go_up s2

  (* Scala dwie dane (rozłączne) klasy. *)
  let union x y = 
    let fx = go_up x 
    and fy = go_up y
    in
      if not (fx == fy) then begin
	if fy.rank > fx.rank then begin
	  fx.up := fy;
          fy.next <- fx :: fy.next
        end else begin
	  fy.up := fx;
	  fx.next <- fy :: fx.next;
	  if fx.rank = fy.rank then fx.rank <- fy.rank + 1
	end;
	sets_counter := !sets_counter - 1
      end
  
  (* Lista elementów klasy. *)
  let elements s = 
    let acc = ref []
    in
      let rec traverse s1 = 
	begin
	  acc := s1.elem :: !acc;
	  List.iter traverse s1.next
	end
      in begin
	traverse (go_up s);
	!acc
      end

end;;

open Find_Union
open Printf

let st a =
	match a with
	| (x,_,_) -> x

let nd a =
	match a with
	| (_, x, _) -> x

let rd a =
	match a with
	| (_, _, x) -> x



let sadzawka tab start =
	let n = Array.length tab
	and m = Array.length tab.(0) 
	and wyn = ref 0 
	and indeks = ref 0 in

	let lol = make_set (-1, -1) in

	let xd = Array.make (n * m) (0,0,0)
	and rep = Array.make_matrix n m lol in

	let brzeg = make_set (n,m) in
	let poczatek = make_set start in

	for i = 0 to n - 1 do 
		for j = 0 to m - 1 do 
			xd.(!indeks) <- (tab.(i).(j), i, j);
			rep.(i).(j) <- make_set (i,j);
			incr indeks;
		done;
	done;

	Array.sort compare xd;
	indeks := !indeks - 1;

	while !indeks >= 0 && equivalent brzeg rep.(fst start).(snd start) = false do
		let a = nd xd.(!indeks) 
		and b = rd xd.(!indeks) in

		printf "%d " a;
		printf "%d\n" b;

		let akt = rep.(a).(b) in
		
		if a = (n - 1) || b = (m - 1) || a = 0 || b = 0 then
			union akt brzeg;

		if tab.(min (a + 1) (n - 1)).(b) > tab.(a).(b) then
			union rep.(min (a + 1) (n - 1)).(b) akt;

		if tab.(max (a - 1) 0).(b) > tab.(a).(b) then
			union rep.(max (a - 1) 0).(b) akt;

		if tab.(a).(min (b + 1) (m - 1)) > tab.(a).(b) then
			union rep.(a).(min (b + 1) (m - 1)) akt;

		if tab.(a).(max (b - 1) 0) > tab.(a).(b) then
			union rep.(a).(max (b - 1) (m - 1)) akt;

		wyn := tab.(a).(b);
		indeks := !indeks - 1;
	done;
	
	!wyn





































