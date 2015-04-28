
open Core.Std

let rotors = [1; 2; 3; 4; 5; 6; 7; 8]
let total_rotors = 3

(* run on utop and type in (rotor_possibilities total_rotors rotors)*)
let rotor_possibilities (k: int) list = 
    let rec aux (k) (acc) (emit) = function
      | [] -> acc
      | h :: t ->
        if k = 1 then aux k (emit [h] acc) emit t else
          let new_emit x = emit (h :: x) in
          aux k (aux (k-1) acc new_emit t) emit t
    in
    let emit (x) (acc) = x :: acc in
    aux k [] emit list;;
;;

