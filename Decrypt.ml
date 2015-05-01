open Core.Std
open Printf
open Enigma4
open quadgrams

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
(*Special version of enigma function for decoding*)
let enigma_b (message: bytes) (rotors: rotor list) (grund: int list) (ringstel: int list) : bytes =
    (* VALIDATE INPUT *)
    if List.length grund <> 3 
    then failwith "\nEnigma: Initial position of rotors (Grundstellung) is invalid. Provide 3 letters."
    else if List.length ringstel <> 3
    then failwith "\nEnigma: Initial position of rings (Ringstellung) is invalid. Provide 3 letters."
    else
    (* INITIALIZATION: Run Ringstellung and Grundstellung *)
    let new_rotors = grundstellung (ringstellung rotors ringstel) grund in
    (* ENCRYPT THE MESSAGE *)
    let encrypted_message = string_enigma message new_rotors in
    (* RETURN BACK TO STRING *)
    decode_string encrypted_message



(* THIS IS FOR DEBUGGING ONLY IT HAS TO BE CHANGED TO AN ACTUAL FUNCTION*)
let karen_function (mess: bytes) : float =
score_calculator mess
     
(*Makes a list of all combinations of rotors*)
let all_pos = create rotors rotors rotors reflector
(* Generates all the grund possibilities, runs karen_function on them and returns
 the setting with the highest score *)      
let rec grundfeeder (messag: bytes) (rotor_pos: rotor list list) (sett: (int*int*int)) (score: float) (highsett: int list) (highrotor: rotor list): (int list * rotor list) =
  match rotor_pos with
  |[] -> (highsett, highrotor)
  |[hd] ->
    (match sett with
    |(a,b,c) -> 
      if c = 27 then grundfeeder messag rotor_pos (a,(b+1),1) score highsett highrotor
      else if b = 27 then grundfeeder messag rotor_pos ((a+1), 1, 1) score highsett highrotor
      else if a = 27 then (highsett, highrotor)
      else let current_score = karen_function (enigma_b messag hd [a; b; c] [1;1;1]) in
	   if score < current_score then grundfeeder messag rotor_pos (a,b,(c+1)) current_score [a;b;c] hd
	   else grundfeeder messag rotor_pos (a, b, (c+1)) score highsett highrotor
    )
  |hd::tl ->
        (match sett with
    |(a,b,c) -> 
      if c = 27 then grundfeeder messag rotor_pos (a,(b+1),1) score highsett highrotor
      else if b = 27 then grundfeeder messag rotor_pos ((a+1), 1, 1) score highsett highrotor
      else if a = 27 then grundfeeder messag tl (1,1,1) score highsett highrotor
      else let current_score = karen_function (enigma_b messag hd [a; b; c] [1;1;1]) in
	   if score < current_score then grundfeeder messag rotor_pos (a,b,(c+1)) current_score [a;b;c] hd
	   else grundfeeder messag rotor_pos (a, b, (c+1)) score highsett highrotor
    )

(* This function actually breaks stuff. It takes 14s per rotor setting
  so about 14 minutes if only 5 rotors used
  and about 78 minutes if 8 rotors used*)
let grundbreaker (message: bytes) (rotors: rotor list list) : (int list * rotor list) =
  (* Gives the time estimation based on a performance*)
(*
  let e1 = Unix.time() in
  let _ = grundfeeder message [[r33a; r22a; r11a; reflector]] (1,1,1) (-99999999.) [0;0;0] [] in
     let e2 = Unix.time() in
     let rotor_length = float (List.length rotors) in
  let estimate = ((e2 -. e1) *. (rotor_length) *. (rotor_length -.1.) *. (rotor_length -. 1.)) /. 60. in
  Printf.printf "\nEstimated time: %f minutes \n" estimate;
*)
(*---------------------------------------------------------------------*)

  let t0 = Unix.time() in
  let outcome = grundfeeder message rotors (*all_rotors*) (1,1,1) (-99999999.) [0;0;0] [] in
     let t1 = Unix.time() in
     let delta = t1 -. t0 in
     let _ = Printf.printf "\nFinding rotors and grundstellung time: %f seconds \n" delta in
     outcome

(* TESTING *)
(*let shortrotors = 
  List.map [(r1, n1); (r2, n2); (r3, n3); (r4, n4)] ~f:(fun x -> 
    match x with
    |(r, n) -> make_rotor r alphabet n 1)
let supershort = [[r33a;r22a;r11a;reflector]]
let a = grundbreaker "ERRJKOCSN" supershort;;
*)

(*Generates all needed ringsettings and runs karen_function and returns the setting with the
 highest score*)
let ringfeeder (messag: bytes) (grundsettings: (int list * rotor list)): int list =
    match grundsettings with
    |(init, rot) -> 
    let min = -9999999999. in
    let rec slow_ring (best: int) (n: int) (score: float): (int*int*int) =
    if n = 27 then (best, 1, 1)
    else let current_score = karen_function (enigma_b messag rot init [n;1;1]) in
    if current_score > score then slow_ring n (n+1) current_score
    else slow_ring best (n+1) score in
    let rec med_ring (sett: (int*int*int)) (best:int) (score:float) : (int*int*int) =
    match sett with
    |(a,b,c) -> if b = 27 then (a, best, c)
        else let current_score = karen_function (enigma_b messag rot init [a;b;c]) in
        if current_score > score then med_ring (a, b+1, c) b current_score
        else med_ring (a, b+1, c) best score in
    let rec fast_ring (sett: (int*int*int)) (best:int) (score:float) : (int*int*int) =
    match sett with
    |(a,b,c) -> if c = 27 then (a, b, best)
        else let current_score = karen_function (enigma_b messag rot init [a;b;c]) in
        if current_score > score then fast_ring (a, b, c+1) c current_score
        else fast_ring (a, b, c+1) best score in
    match fast_ring (med_ring (slow_ring 1 1 min) 1 min) 1 min with
    |(a, b, c) -> [a;b;c]

(* CHECK *)
let rotorsshort = 
  List.map [(r1, n1); (r2, n2); (r3, n3)] ~f:(fun x -> 
    match x with
    |(r, n) -> make_rotor r alphabet n 1)

  let break_enigma (messag: bytes) : bytes =

  let t00 = Unix.time() in
    let rotors = create rotorsshort rotorsshort rotorsshort reflector in
    let rotor_and_grund = grundbreaker messag  rotors in
    match rotor_and_grund with
    |(grund, rotor) ->
      let ring = ringfeeder (enigma_b messag rotor grund [1;1;1]) rotor_and_grund in
      let outcome = enigma_b messag rotor grund ring in
      let t11 = Unix.time() in
      let delta = t11 -. t00 in
      let _ = Printf.printf "\nTotal time: %f seconds \n" delta in
      outcome


(* TESTING *)
let broken = break_enigma "VFQRDXTWHXUJQEJWGUSHOEJEDBMSFXPDBSCPPSUWXVBDH";;

(*
let shortrotors = 
  List.map [(r1, n1); (r2, n2); (r3, n3); (r4, n4)] ~f:(fun x -> 
    match x with
    |(r, n) -> make_rotor r alphabet n 1)
let supershort = [[r33a;r22a;r11a;reflector]]
let a = grundbreaker "ERRJKOCSN" supershort;;
let foundrtr = match a with
|(_, rtr) -> rtr
let foundgrund = match a with
|(gr, _) -> gr
let message = "QWERTY"
let first_stage = enigma_b message foundrtr foundgrund [1;1;1]
let foundring = ringfeeder (enigma_b message foundrtr foundgrund [1;1;1]) a
let last_stage = enigma_b message foundrtr foundgrund foundring;;
*)
