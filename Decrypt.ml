open Core.Std
open Printf
open Enigma4

let rotors = 
  List.map [(r1, n1); (r2, n2); (r3, n3); (r4, n4); (r5, n5); (r6, n6); (r7, n7); (r8, n8)] ~f:(fun x -> 
    match x with
    |(r, n) -> make_rotor r alphabet n 1)

let reflector = make_rotor u2 alphabet [] 1

(* run on utop and type in: create rotors rotors rotors*)
let rec create (start: 'a list) (left: 'a list) (right: 'a list) (refl: 'a): 'a list list=
let rec list_of_two (x: 'a) (y: 'a) (l: 'a list) : 'a list list=
  match l with
  |[] -> []
  |[hd] -> if hd = x || hd = y then []
    else [[x; y; hd; refl]]
  |hd::tl -> if hd = x || hd = y then list_of_two x y tl
    else [x; y; hd; refl] :: (list_of_two x y tl) in
  match left with
  |[] -> []
  |[hd1] ->
    (match right with
    |[] -> []
    |[hd2] -> if hd1 = hd2 then []
      else (list_of_two hd1 hd2 start) @ (create start [] [] refl)
    |hd2::tl2 -> if hd1 = hd2 then create start left tl2 refl
      else (list_of_two hd1 hd2 start) @ (create start left tl2 refl)
     )
  |hd1::tl1 ->
    (match right with
    |[] -> []
    |[hd2] -> if hd1 = hd2 then create start tl1 start refl
      else (list_of_two hd1 hd2 start) @ (create start tl1 start refl)
    |hd2::tl2 -> if hd1 = hd2 then create start left tl2 refl
      else (list_of_two hd1 hd2 start) @ (create start left tl2 refl)
    )

(* THIS IS FOR DEBUGGING ONLY IT HAS TO BE CHANGED TO AN ACTUAL FUNCTION*)
let karen_function (mess: bytes) : float =
  Printf.printf "(%d, %d, %d) " a b c;
      0.0
      
(* Generates all the grund possibilities, runs karen_function on them and returns
 the setting with the highest score *)      
let rec grundfeeder (sett: (int*int*int)) (score: float) (highsett: (int*int*int)): (int*int*int) =
  match sett with
  |(a,b,c) -> 
    if c = 27 then grundfeeder (a,(b+1),1) score highsett
    else if b = 27 then grundfeeder ((a+1), 1, 1) score highsett
    else if a = 27 then highsett
    else let current_score = karen_function sett in
    if score < current_score then grundfeeder (a,b,(c+1)) current_score sett
    else grundfeeder (a, b, (c+1)) score highsett 

let _ = grundfeeder (1,1,1) (-9999999.) (0,0,0);;

(*Generates all needed ringsettings and runs karen_function and returns the setting with the
 highest score*)
let ringfeeder : (int*int*int) =
    let min = -9999999999. in
    let rec slow_ring (best: int) (n: int) (score: float): (int*int*int) =
    if n = 27 then (best, 1, 1)
    else let current_score = karen_function (n, 1, 1) in
    if current_score > score then slow_ring n (n+1) current_score
    else slow_ring best (n+1) score in
    let rec med_ring (sett: (int*int*int)) (best:int) (score:float) : (int*int*int) =
    match sett with
    |(a,b,c) -> if b = 27 then (a, best, c)
        else let current_score = karen_function sett in
        if current_score > score then med_ring (a, b+1, c) b current_score
        else med_ring (a, b+1, c) best score in
    let rec fast_ring (sett: (int*int*int)) (best:int) (score:float) : (int*int*int) =
    match sett with
    |(a,b,c) -> if c = 27 then (a, b, best)
        else let current_score = karen_function sett in
        if current_score > score then fast_ring (a, b, c+1) c current_score
        else fast_ring (a, b, c+1) best score in
    fast_ring (med_ring (slow_ring 1 1 min) 1 min) 1 min
