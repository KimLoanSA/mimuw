type 'a queue
(** Typ złączalnej kolejki priorytetowej *)

val empty : 'a queue
(** Pusta kolejka priorytetowa *)

val add : 'a -> 'a queue -> 'a queue
(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
    do kolejki [q] *)

exception Empty
(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)

val delete_min : 'a queue -> 'a * 'a queue
(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)

val join : 'a queue -> 'a queue -> 'a queue
(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)

val is_empty : 'a queue -> bool
(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)