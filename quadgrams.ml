open Core.Std
open Printf

(*Declaring Tree type*)

type pair = bytes list * int

type tree = 
| Leaf
| Two of tree * pair * tree
| Three of tree * pair * tree * pair * tree

type kicked =
  | Up of tree * pair * tree
  | Done of tree

let empty : tree = Leaf

let string_compare x y =
  let i = String.compare x y in
    if i = 0 then Equal else if i < 0 then Less else Greater ;;


(*extracting quadgram pairs from text. THIS IS FOR DECRYPTED TEXT*)

let rec quadgrams (text: bytes) (n: int) : bytes list = 
  if n <= ((String.length text) - 4) then  
  ((Char.escaped text.[n])^(Char.escaped text.[n + 1])^(Char.escaped text.[n + 2])^(Char.escaped text.[n + 3]))::quadgrams text (n + 1)
  else []

let quad_compare_sans_numbers (quadgram1 : 'a list) (quadgram2 : 'a list) = 
  let ([c1;c2;c3;c4], [s1;s2;s3;s4]) = (quadgram1, quadgram2) in
  match string_compare c1 s1 with
  |Less -> Less
  |Greater -> Greater
  |Equal ->
    match string_compare c2 s2 with
    |Less -> Less
    |Greater -> Greater
    |Equal ->
      match string_compare c3 s3 with
      |Less -> Less
      |Greater -> Greater
      |Equal ->
	match string_compare c4 s4 with
	|Less -> Less
	|Greater -> Greater
	|Equal -> Equal 


let rec lookup (quad_list : bytes list) (t: tree) = 
  match quad_list, t with
  |[], Leaf -> None
  |_::_, Leaf -> None
  |hd::tl, Two(l, (q1, f1), r) ->
    (match quad_compare_sans_numbers quad_list q1 with
    |Less -> lookup quad_list l
    |Greater -> lookup quad_list r
    |Equal -> Some f1) 
  |hd::tl, Three(l, (q1, f1), m, (q2, f2), r) -> 
    (match quad_compare_sans_numbers quad_list q1 with
    |Less -> lookup quad_list l
    |Equal -> Some f1
    |Greater -> 
      (match quad_compare_sans_numbers quad_list q2 with
      |Less -> lookup quad_list m
      |Greater -> lookup quad_list r
      |Equal -> Some f2))



(*For loading quadgrams and their frequencies from text file, provides a list of the actual quadgram*)

let rec quadgram_getter (l: 'a list) (n: int) : 'a list = 
  match l with
  |[] -> []
  |hd::tl -> 
    if n <= 3 
    then hd::(quadgram_getter tl (n + 1))
    else [] 

(*For loading quadgrams and their frequencies from text file, provies list of the frequency*)

let rec frequency_getter (l: 'a list) (n: int) : 'a list = 
  match l with
  |[] -> []
  |hd::tl ->
    if n <= 3
    then frequency_getter tl (n + 1)
    else tl

(*Converts char ints into actual ints. For frequency use only*)

let make_me_int (ilist : 'a list) = 
  let rec actual_frequency (ilist: 'a list) = 
    match ilist with
    |[] -> ""
    |hd::tl -> (Char.escaped hd)^(actual_frequency tl) 
  in int_of_string (actual_frequency ilist)

(*Function that combines functionality of above helper functions. Call only on quads and freqs from text file; returns a tuple of a string list and an int*)

let separator (s: string) (n: int) : ('a list * int) = 
  let char_list = String.to_list s in
    (quadgram_getter (List.map char_list (fun x -> Char.escaped x)) n, make_me_int(frequency_getter char_list n))





(*insertion into tree*)

let string_compare x y =
  let i = String.compare x y in
    if i = 0 then Equal else if i < 0 then Less else Greater ;;

let quad_compare (quadgram1: ('a list * int)) (quadgram2: ('a list * int)) = 
  let (([c1;c2;c3;c4], _), (([s1;s2;s3;s4]), _)) = (quadgram1, quadgram2) in
  match string_compare c1 s1 with
  |Less -> Less
  |Greater -> Greater
  |Equal ->
    match string_compare c2 s2 with
    |Less -> Less
    |Greater -> Greater
    |Equal ->
      match string_compare c3 s3 with
      |Less -> Less
      |Greater -> Greater
      |Equal ->
	match string_compare c4 s4 with
	|Less -> Less
	|Greater -> Greater
	|Equal -> Equal

let insert_upward_two (w: pair) (w_left: tree) (w_right: tree) (x:pair) (x_other: tree) : kicked =
  if quad_compare w x = Less
  then Done(Three(w_left, w, w_right, x, x_other))
  else Done(Three(x_other, x, w_left, w, w_right))

let insert_upward_three (w: pair) (w_left: tree) (w_right: tree) (x: pair) (y: pair) (other_left: tree) (other_right: tree) : kicked = 
  if quad_compare w x = Less
  then Up(Two(w_left, w, w_right), x, Two(other_left, y, other_right))
  else if quad_compare w y = Greater
  then Up(Two(other_left, x, other_right), y, Two(w_left, w, w_right))
  else Up(Two(other_left, x, w_left), w, Two(w_right, y, other_right))

let rec insert_downward (t: tree) (p: pair) : kicked = 
  match t with
  |Leaf -> Up(Leaf, p, Leaf)
  |Two(left, n, right) -> insert_downward_two p n left right
  |Three(left, n1, middle, n2, right) -> insert_downward_three p n1 n2 left middle right

and insert_downward_two (p: pair) (p1: pair) (left: tree) (right: tree) : kicked = 
  if quad_compare p p1 = Less
  then match insert_downward left p with
  |Up(l, kp, r) -> insert_upward_two kp l r p1 right
  |Done(d) -> Done(Two(d, p1, right))
  else if quad_compare p p1 = Greater
  then match insert_downward right p with
  |Up(l, kp, r) -> insert_upward_two kp l r p1 left
  |Done(d) -> Done(Two(left, p1, d))
  else Done(Two(left, p, right))

and insert_downward_three (p: pair) (p1: pair) (p2: pair) (left: tree) (middle: tree) (right: tree) : kicked = 
  if quad_compare p p1 = Less
  then match insert_downward left p with
  |Up(l, kp, r) -> insert_upward_three kp l r p1 p2 middle right
  |Done(d) -> Done(Three(d, p1, middle, p2, right))
  else if quad_compare p p2 = Greater 
  then match insert_downward right p with
  |Up(l, kp, r) -> insert_upward_three kp l r p1 p2 left middle
  |Done(d) -> Done(Three(left, p1, middle, p2, d))
  else if quad_compare p p1 = Equal
  then Done(Three(left, p, middle, p2, right))
  else if quad_compare p p2 = Equal
  then Done(Three(left, p1, middle, p, right))
  else match insert_downward middle p with
  |Up(l, kp, r) -> insert_upward_three kp l r p1 p2 left right
  |Done(d) -> Done(Three(left, p1, d, p2, right))

let insert (t: tree) (p: pair) : tree = 
  match insert_downward t p with
  |Up(l, p1, r) -> Two(l, p1, r)
  |Done x -> x






(*Reading in from a file*)


let line_stream_of_channel channel =
    Stream.from
      (fun _ ->
         try Some (input_line channel) with End_of_file -> None)

let ultimate_read (n: int) (t: tree) : tree = 
  let file = "english_quadgrams.txt" in
  let in_channel = open_in file in
  let lines = line_stream_of_channel in_channel in
  let rec read (lines: bytes Stream.t) (t: tree) (n: int) = 
    if n <= 389372 
    then read lines (insert t (separator (Stream.next lines) 0)) (n + 1)
    else t in
  read lines t n;;




(*calculate total number of all quadgrams in text*)


let rec lister (t: tree) : float list = 
  match t with
  |Leaf -> []
  |Two(t1, (_, v1), t2) -> lister t1 @ [(float v1) /. 1000.] @ lister t2
  |Three(t1, (_, v1), t2, (_, v2), t3) -> lister t1 @ [(float v1 /. 1000.)] @ lister t2 @ [(float v2 /. 1000.)] @ lister t3

let total_quadgrams (t: tree) = 
  List.fold_right (lister t) ~f:(fun x y -> (x) +. (y)) ~init: 0. 


(*

  let rec actual_folder (f: key -> value -> 'a -> 'a) (u: 'a) (l: pair list) : 'a = 
    match l with
    |[] -> u
    |(k, v)::tl -> f k v (actual_folder f u tl)
    

  let fold (f: key -> value -> 'a -> 'a) (u: 'a) (d: dict) : 'a =
    let lst_of_dict = lister d in 
    actual_folder f u lst_of_dict
*)
  

let string_of_char = (fun x -> Char.escaped x)


let quadgram_step1 (quadgram_list : bytes list) : char list list =
  List.map quadgram_list (fun x -> String.to_list x) 

let rec quadgram_step2 (char_list_list : char list list) : bytes list list = 
  match char_list_list with
  |[] -> []
  |hd::tl -> (List.map hd string_of_char)::(quadgram_step2 tl) 

let quadgram_step3 (bytes_list_list : bytes list list) (t: tree) = 
  List.map bytes_list_list (fun x -> lookup x t)

let quadgram_tree  = ultimate_read 0 empty
let total_number_of_quadgrams = total_quadgrams quadgram_tree
let floored_log_value = log (0.001 /. (total_number_of_quadgrams))


let score_calculator (text: string) = 
  let quadgram_list = quadgrams (String.uppercase text) 0 in
  let step1 = quadgram_step1 quadgram_list in
  let step2 = quadgram_step2 step1 in 
  let option_freq_values = quadgram_step3 step2 quadgram_tree in
  let with_zero_freq_values = List.map option_freq_values ~f:(fun x -> match x with |None -> 0 |Some y -> y) in
  let rec scaler (lst: int list) = 
  match lst with
  |[] -> []
  |hd::tl -> ((float hd) /. 1000.)::(scaler tl) in
  let freq_values = scaler with_zero_freq_values in
  let probabilities = List.map freq_values ~f:(fun x -> if x = 0. then floored_log_value else log (x /. (total_number_of_quadgrams))) in
  List.fold_right probabilities ~f:(fun x y -> x +. y) ~init: 0.

  
  
