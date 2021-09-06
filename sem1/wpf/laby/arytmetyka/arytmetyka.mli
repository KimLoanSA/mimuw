(************************************************)
(* Zadanie o arytmetyce niedokładnych wartości. *)
(************************************************)

(* Typ reprezentujący niedokładne wartości. *)
type wartosc 

(* Implicite zakładamy, że wszystkie argumenty typu float są liczbami *)
(* rzeczywistymi, tzn. są różne od infinity, neg_infinity i nan.      *)


(* wartosc_dokladnosc x p = x +/- p% *)
(* war.pocz.: p > 0                  *)
val wartosc_dokladnosc: float -> float -> wartosc    

(* wartosc_od_do x y = [x;y]         *)
(* war.pocz.: x <= y                 *)
val wartosc_od_do: float -> float -> wartosc                            

(* wartosc_dokladna x = [x;x]        *)
val wartosc_dokladna: float -> wartosc   

(* in_wartosc w x = x \in w *)
val in_wartosc: wartosc -> float -> bool 

(* min_wartosc w = najmniejsza możliwa wartość w,   *)
(* lub neg_infinity jeśli brak dolnego ograniczenia.*)
val min_wartosc: wartosc -> float       

(* max_wartosc w = największa możliwa wartość w,    *)
(* lub infinity jeśli brak górnego ograniczenia.    *)
val max_wartosc: wartosc -> float       

(* środek przedziału od min_wartosc do max_wartosc, *)
(* lub nan jeśli min i max_wartosc nie są określone.*)
val sr_wartosc:  wartosc -> float       

(* Operacje arytmetyczne na niedokładnych wartościach. *)
val plus:      wartosc -> wartosc -> wartosc  
val minus:     wartosc -> wartosc -> wartosc 
val razy:      wartosc -> wartosc -> wartosc 
val podzielic: wartosc -> wartosc -> wartosc
