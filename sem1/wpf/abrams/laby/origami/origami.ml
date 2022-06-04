(* origami *)
(* Marcin Abramowicz *)
(* code review: Franciszek Hnatów *)

(* zlozonosc: pesymistycznie O(2^n), gdzie n to liczba zlozen kartki *)
(**************************************************************************************************************************************************************************************)
(* reprezentacja *)

(* punkt - para wspolrzednych *)
type point = float * float

(* kartka - funkcja przyjmujaca punkt i zwracajaca ile razy po zlozeniach przebija on kartke *)
type kartka = point -> int

(**************************************************************************************************************************************************************************************)
(* porownywanie z epsilonem *)

let eps = 0.0000000001

let (<=) a b =
	(a -. eps) < b

let (<=>) a b =
	(a -. eps < b) && (a +. eps > b)

(**************************************************************************************************************************************************************************************)
(* funkcje pomocnicze *)

(* funkcja zwracajaca iloczyn wektorowy wektorow *)
let det (ax, ay) (bx, by) = 
	ax *. by -. ay *. bx

(* funkcja zwracajaca roznice dwoch wektorow *)
let odejmij (ax, ay) (bx, by) =
	(ax -. bx, ay -. by)

(* funkcja zwracajaca kwadrat liczby *)
let do_kwadratu a =
	a *. a

(* funkcja zwraca prosta przechodzaca przez zadane punkty *)
let prosta (ax, ay) (bx, by) = 
	let a = (by -. ay) /. (bx -. ax) in
	let b = ay -. (ax *. a) in
	(a, b)

(* funkcja zwraca prosta o danym wspolczyniku a i przechodzaca przez punkt *)
let prosta_a a (px, py) = 
	(a, py -. (a *. px))

(* funkcja zwraca punkt odbity wzgledem prostej *)
let odbicie (ax, ay) (bx, by) (px, py) =
	if ax = bx then (* prosta w postaci x = cos *)
		(2. *. ax -. px, py)
	else if ay = by then (* prosta w postaci y = cos *)
		(px, 2. *. ay -. py)
	else let (wsp_a1, wsp_b1) = prosta (ax, ay) (bx, by) in (* obliczamy prosta przechodzaca przez dane punkty *)
		let (wsp_a2, wsp_b2) = prosta_a (-.1. /. wsp_a1) (px, py) in (* obliczamy prosta prostopadla do danej i przechodzaca przez dany punkt *)
		let qx = (wsp_b2 -. wsp_b1) /. (wsp_a1 -. wsp_a2) in (* obliczamy punkt przeciecia sie prostych *)
		let qy = qx *. wsp_a1 +. wsp_b1 in 
			(2. *. qx -. px, 2. *. qy -. py) (* zwracamy odbicie, czyli przesuniecie punktu o wektor p - q *)

(**************************************************************************************************************************************************************************************)
(* funkcje *)

(* funkcja zwraca prostokatna kartke okreslona przez punkty *)
let prostokat (ax, ay) (bx, by) = fun (px, py) ->
	if ax <= px && px <= bx && ay <= py && py <= by then 1
	else 0

(* funkcja zwraca okragla kartke zadana przez srodek i promien *)
let kolko (ax, ay) r = fun (px, py) ->
	if do_kwadratu (px -. ax) +. do_kwadratu (py -. ay) <= do_kwadratu r then 1
	else 0

(* funkcja skladajaca kartke wedlug punktow *)
let zloz a b kart = fun p ->
	let d = det (odejmij b a) (odejmij p a) in (* iloczyn wektorowy wektorow [bx-ax;by-ay] [px-ax;py-ay] *)
	if d <=> 0. then (* punkt lezy na prostej, wiec liczba zgiec taka jak byla *)
		kart p
	else if d <= 0. then (* punkt lezy po prawej stronie prostej *)
		0 
	else (* punkt lezy po lewej stronie prostej, wiec odbijamy go *)
		kart p + kart (odbicie a b p)

(* funkcja skladajaca kartke wedlug punktow na liscie *)
let skladaj l kart =
	List.fold_left (fun acc (a, b) -> zloz a b acc) kart l
;;

(**************************************************************************************************************************************************************************************)
(* testy *)

(*
let op=[((10.0,8.0),(0.0,4.0));((0.0,0.0),(1.0,10.0));((3.0,9.0),(4.0,7.0));((2.0,1.0),(9.0,9.0));((2.0,10.0),(6.0,4.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test77=skladaj op kartka;;
assert (test77 (10.0,10.0)=0);;
let op=[((0.0,5.0),(9.0,8.0));((6.0,10.0),(8.0,0.0));((3.0,8.0),(0.0,7.0));((0.0,7.0),(9.0,6.0));((10.0,3.0),(1.0,7.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test78=skladaj op kartka;;
assert (test78 (4.0,10.0)=0);;
let op=[((4.0,4.0),(5.0,9.0));((4.0,5.0),(1.0,9.0));((3.0,6.0),(1.0,5.0));((2.0,7.0),(9.0,6.0));((2.0,2.0),(7.0,6.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test79=skladaj op kartka;;
assert (test79 (10.0,0.0)=0);;
let op=[((2.0,10.0),(4.0,3.0));((5.0,5.0),(9.0,1.0));((2.0,5.0),(0.0,9.0));((5.0,6.0),(1.0,2.0));((4.0,3.0),(1.0,7.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test80=skladaj op kartka;;
assert (test80 (6.0,4.0)=0);;
let op=[((5.0,7.0),(2.0,3.0));((10.0,9.0),(1.0,0.0));((0.0,9.0),(5.0,2.0));((6.0,4.0),(10.0,5.0));((5.0,1.0),(3.0,7.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test81=skladaj op kartka;;
assert (test81 (0.0,4.0)=0);;
let op=[((6.0,10.0),(5.0,4.0));((9.0,1.0),(3.0,9.0));((1.0,3.0),(9.0,2.0));((7.0,4.0),(5.0,1.0));((9.0,5.0),(1.0,1.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test82=skladaj op kartka;;
assert (test82 (3.0,0.0)=0);;
let op=[((1.0,0.0),(6.0,8.0));((2.0,0.0),(2.0,3.0));((2.0,4.0),(1.0,1.0));((5.0,0.0),(1.0,3.0));((1.0,0.0),(0.0,6.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test83=skladaj op kartka;;
assert (test83 (2.0,8.0)=0);;
let op=[((1.0,8.0),(0.0,5.0));((3.0,1.0),(1.0,6.0));((8.0,4.0),(5.0,7.0));((10.0,8.0),(3.0,1.0));((0.0,6.0),(7.0,10.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test84=skladaj op kartka;;
assert (test84 (3.0,9.0)=0);;
let op=[((4.0,6.0),(2.0,0.0));((0.0,7.0),(3.0,5.0));((9.0,6.0),(2.0,6.0));((8.0,6.0),(2.0,5.0));((2.0,8.0),(6.0,9.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test85=skladaj op kartka;;
assert (test85 (8.0,7.0)=0);;
let op=[((8.0,3.0),(9.0,1.0));((2.0,5.0),(7.0,4.0));((2.0,1.0),(0.0,7.0));((9.0,10.0),(10.0,6.0));((5.0,7.0),(9.0,3.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test86=skladaj op kartka;;
assert (test86 (9.0,2.0)=0);;
let op=[((6.0,4.0),(3.0,10.0));((10.0,5.0),(7.0,4.0));((1.0,10.0),(4.0,10.0));((5.0,3.0),(10.0,9.0));((1.0,8.0),(7.0,4.0))];;
let kartka=prostokat (0.,0.) (10.,10.) ;;
let test87=skladaj op kartka;;
assert (test87 (0.0,10.0)=0);;
let op=[((1.0,8.0),(5.0,10.0));((2.0,7.0),(9.0,3.0));((0.0,10.0),(7.0,0.0));((10.0,5.0),(6.0,9.0));((0.0,1.0),(8.0,2.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test88=skladaj op kartka;;
assert (test88 (1.0,9.0)=1);;
let op=[((10.0,9.0),(4.0,0.0));((6.0,2.0),(2.0,0.0));((8.0,2.0),(0.0,2.0));((7.0,3.0),(8.0,5.0));((10.0,4.0),(5.0,5.0))];;
let kartka=kolko (5.,5.) 4. ;;
let test89=skladaj op kartka;;
assert (test89 (5.0,8.0)=0);;
*)