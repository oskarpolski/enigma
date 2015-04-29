
open Core.Std

let rotors = [1; 2; 3; 4; 5; 6; 7; 8]
let total_rotors = 3

let make_list k: ('a*'a*'a) : 'a list =
  match k with
  |(a, b, c) -> [a; b; c]

(* run on utop and type in: create rotors rotors rotors*)
let rec create (start: 'a list) (left: 'a list) (right: 'a list) : 'a list list=
let rec list_of_two (x: 'a) (y: 'a) (l: 'a list) : 'a list list=
  match l with
  |[] -> []
  |[hd] -> if hd = x || hd = y then []
    else [[x; y; hd]]
  |hd::tl -> if hd = x || hd = y then list_of_two x y tl
    else [x; y; hd] :: (list_of_two x y tl) in
  match left with
  |[] -> []
  |[hd1] ->
    (match right with
    |[] -> []
    |[hd2] -> if hd1 = hd2 then []
      else (list_of_two hd1 hd2 start) @ (create start [] [])
    |hd2::tl2 -> if hd1 = hd2 then create start left tl2
      else (list_of_two hd1 hd2 start) @ (create start left tl2)
     )
  |hd1::tl1 ->
    (match right with
    |[] -> []
    |[hd2] -> if hd1 = hd2 then create start tl1 start
      else (list_of_two hd1 hd2 start) @ (create start tl1 start)
    |hd2::tl2 -> if hd1 = hd2 then create start left tl2
      else (list_of_two hd1 hd2 start) @ (create start left tl2)
    )
