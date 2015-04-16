(*
 * Enigma.ml is a program developed by Oskar Kocol as a part of CS51 final project
 * about "Using quadgram anlysis to break German Enigma code" along with
 * Karen Mardini, Elizabeth Elmgren, Timothy Makalinao.
 *
 * email: kocol at college.harvard.edu
 *
 * The code below implements German Enigma I withouth a plug board
 *
 * For use: please type in 
 * enigma "your message" "rotors you use" "starting position" "rotor setting"
 *
 *)

open Core.Std
(* We use this record to store values about a rotor.
    -comb: is a current substitution string (which is going to be changed
        every encryption of a letter)

    -inverse:is an inverse encoding string which is going to be
        created based on comb. It works such that if in comb A ­> Z then in
        inverse Z ­> A;

    -notch: is a hardcoded list of characters which trigger next rotor to
        rotate (the rotor on the left in real Enigma)

    -current:is the current position of a rotor illustrated by a
        character

    -alpha: is alphabet which is going to be altered every encryption
        similarly to comb ­ rotor starting from position ‘A’ is going to start
        with “ABCDEF...XYZ” and after encryption of one letter alpha is
        going to change to “BCDEF...XYZA”. *)
type rotor = {comb: int list; inverse: int list; notch: int list; current: int; alpha: int list}

(* === HELPER FUNCTIONS === *)
(* Takes in a string, and coverts it to ASCII - 64 
    omitting all what is not alphanumeric*)
let encode_string (s: bytes) : int list =
let input_char = String.to_list (String.uppercase s) in
let values = List.map input_char ~f:(fun x -> (Char.to_int x - 64)) in
List.filter values ~f:(fun x -> 0 < x && x < 27)

(* Checks whether a letter wraps around the clock *)
let check_position (i: int) : int =
	if i > 26 then i mod 26
	else if i = 0 then 26
	else i

(* Parses int list back to a string.
    Used for printing the final message *)
let decode_string (s: int list) : bytes =
	List.fold_right s ~f:(fun x acc -> (Char.to_string (char_of_int (x+64)))^acc) ~init: "" ;;

(* Checks whether an element k is in the list l *)
let exists (k: int) (l: int list) =
    List.fold_left l ~init: false ~f:(fun b x -> b || x = k)

(* Finds the nth element of the list lst *)
let rec find_nth (lst: 'a list) (n: int) : 'a =
	match lst with
	|[] -> failwith "Find_nth: run out of list"
	| hd::tl -> if n = 1 then hd else find_nth tl (n-1)

(* Finds what is the index of a letter n in the list *)
let which_nth (lst:int list) (n:int) : int =
	let rec helper (l: int list) (n: int) (current: int) =
	match l with
	|[] -> failwith "Which_nth failed"
	|[hd] -> if hd = n then current else helper [] n current+1
	|hd::tl -> if hd = n then current else helper tl n (current+1) in
	helper lst n 1

(* === PREPARE DATA BASED ON REAL GERMAN CODE === *)
let alphabet = encode_string "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"
(* UPDATE ROTORS *)									  
let r1 = encode_string "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let r2 = encode_string "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let r3 = encode_string "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let r4 = encode_string "ESOVPZJAYQUIRHXLNFTGKDCMWB"
let r5 = encode_string "VZBRGITYUPSDNHLXAWMJQOFECK"
let r6 = encode_string "JPGVOUMFYQBENHZRDKASXLICTW"
let r7 = encode_string "NZJHGRCXMYSWBOUFAIVLPEKQDT"
let r8 = encode_string "FKQHTLXOCBJSPDZRAMEWNIUYGV"
(* === INVERTED ROTORS === *)
let i1 = encode_string "UWYGADFPVZBECKMTHXSLRINQOJ"
let i2 = encode_string "AJPCZWRLFBDKOTYUQGENHXMIVS"
let i3 = encode_string "TAGBPCSDQEUFVNZHYIXJWLRKOM"
let i4 = encode_string "HZWVARTNLGUPXQCEJMBSKDYOIF"
let i5 = encode_string "QCYLXWENFTZOSMVJUDKGIARPHB"
let i6 = encode_string "SKXQLHCNWARVGMEBJPTYFDZUIO"
let i7 = encode_string "QMGYVPEDRCWTIANUXFKZOSLHJB"
let i8 = encode_string "QJINSAYDVKBFRUHMCPLEWZTGXO"
(* === REFLECTORS === *)
(*M3 enigma always uses second reflector *)
let u1 = encode_string "EJMZALYXVBWFCRQUONTSPIKHGD"
let u2 = encode_string "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let u3 = encode_string "FVPJIAOYEDRZXWGCTKUQSBNMHL"
(* === NOTCHES FOR ROTORS === *)
let n1 = encode_string "Q"
let n2 = encode_string "E"
let n3 = encode_string "V"
let n4 = encode_string "J"
let n5 = encode_string "Z"
let n6 = encode_string "ZM"
let n7 = encode_string "ZM"
let n8 = encode_string "ZM"

(* This function takes in values describing a rotor
    and creates a record representing rotor *)
let make_rotor (combination: int list) (alphab: int list) (inotch: int list) (pos: int): rotor =
	let rec find_position (lst: int list) (letter: int) (i: int): int =
	match lst with
	|[] -> failwith "get_inverse: empty list"
	|[hd] -> if hd = letter then i else failwith "get_inverse: not found %d"
	|hd::tl -> if hd = letter then i else find_position tl letter (i+1) in
	let rec get_inverse (rotor: int list) (inv: int list) (l: int) : int list =
	if l > 26  then List.rev inv
	else get_inverse rotor ((find_position rotor l 1)::inv)  (l+1) in
	{comb = combination; inverse = (get_inverse combination [] 1); notch = inotch; current = pos; alpha = alphab}

(* clicker follows the stepping motiong of enigma
    which happens after every encryption
    but only on a single list*)
let clicker (lst: int list) : int list =
  let remove_first (lst: int list) : int list =
	match List.tl lst with
        | None -> failwith "\nclicker: empty list"
		| Some tl-> tl in
  let rec get_first (lst: int list) : int list=
	match List.hd lst with
        | None -> failwith "\n Clicker: empty list 2"
        | Some hd -> [hd] in
  remove_first (lst@(get_first lst))

(* Principe is similar to clicker but this function works
    counterclockwise*)
  let counter_clicker (lst: int list) : int list =
    let rec get_last (l: int list) : int =
    match l with
    |[] -> failwith "\nCounter_clicker: Empty list"
    |[hd] -> hd
    |hd::tl -> get_last tl in
    let rec remove_last (l: int list) : int list =
    match l with
    |[] -> []
    |[hd] -> []
    |hd::tl -> hd::(remove_last tl) in
    remove_last ((get_last lst)::lst)

(* Clicks the first rotor and checks whether other have to be clicked too *)
(* Implements also so called Double Stepping*)
let rec check_rotors (r: rotor list) (n: int) : rotor list =
	let double_stepping (rest: rotor list) (double: bool): rotor list  =
	match rest with
	|[] -> []
	|[hd] -> [hd]
	|second::tl -> let clicked = clicker second.comb in
	if not double 
	then (make_rotor clicked (clicker second.alpha) second.notch (check_position (second.current+1))) :: tl
	else if exists (check_position (second.current)) second.notch 
	then (make_rotor clicked (clicker second.alpha) second.notch (check_position (second.current+1))) :: check_rotors tl 1
	else rest in
	match r with
	|[] -> []
	|[hd] -> (* neglect the reflector which is the last term *)
			[hd]
	|hd::tl -> let clickedhead = clicker hd.comb in
	(* Implements double stepping *)
		if (exists hd.current hd.notch)
		then (make_rotor clickedhead (clicker hd.alpha) hd.notch (check_position (hd.current+1))) :: double_stepping tl false
		else if (exists (check_position (hd.current-1)) hd.notch) 
		then (make_rotor clickedhead (clicker hd.alpha) hd.notch (check_position (hd.current+1))) :: double_stepping tl true
		else (make_rotor clickedhead (clicker hd.alpha) hd.notch (check_position (hd.current+1))) :: tl

(* Encodes one letter (or more specifically its int corespondent)*)
(* It takes into account relative positions of rotors *)
(* IMPORTANT: It has to receive already clicked rotor list *)
let letter_enigma (letter: int) (list_rotors: rotor list) : int =
	let rec run_inv (l: int) (work_rotors: rotor list) (number: int) : int =
		match work_rotors with
		|[] ->  failwith "\nrun_inv: Empty list whereas it shouldn't be"
		|[hd] -> find_nth hd.inverse l
		|hd::tl -> 
				let first = hd.current in
				let next_rotor = find_nth tl 1 in
				let next = next_rotor.current in
				let delta = first - next in
				let tmp = find_nth hd.inverse l in
				(* Adjustment for rotation *)
				let rotor_outcome = (if tmp + (first -1) > 26 then tmp - 26 + (first - 1)
				else tmp+first-1) in
				(* Skip reflector rotor *)
				(*deal with inverse*)
				if number = 1 then run_inv (check_position (l-delta)) tl 2
				else
				(* Next first letter is greater than current rotor's *)  
				if delta > 0 then 
					(if rotor_outcome - delta < 1 then (run_inv (rotor_outcome + (26 - delta)) tl (number+1))
				else (run_inv ((rotor_outcome - delta) mod 26) tl (number+1)))
				(* Next one is the same - normal substitution*)
				else if delta = 0 then run_inv rotor_outcome tl (number+1)

				else (if rotor_outcome + (abs delta) > 26 then (run_inv ((rotor_outcome+ (abs delta)) mod 26) tl (number+1))
				else (run_inv (rotor_outcome + (abs delta)) tl (number+1))) in
	(* runs a letter (ascii - 64) through rotor *)
		let rec run_rotors (l: int) (work_rotors: rotor list) (number: int) : int =
		(*Check if already reached inverse *)
		if number = 5 then run_inv l (List.rev work_rotors) 1
		else match work_rotors with
		|[] ->  l
		|[hd] -> if number = 1 then find_nth hd.comb l 
					else find_nth hd.comb (which_nth hd.alpha l)
		|hd::tl -> 

				(* Check the difference between motors *)
				let rotor_outcome = (if number = 1 then find_nth hd.comb l 
					else find_nth hd.comb (which_nth hd.alpha l)) in
				let first = hd.current in		
				let next_rotor = find_nth tl 1 in
				let next = next_rotor.current in			
				let delta = first - next in
				(* This has to be checked *)
				if delta < 0 then 
					(if rotor_outcome + (abs delta) > 26 then (run_rotors ((rotor_outcome+ (abs delta)) mod 26) tl (number+1))
				else (run_rotors (rotor_outcome + (abs delta)) tl (number+1)))
				else if delta = 0 then run_rotors rotor_outcome tl (number+1)
				else if rotor_outcome > delta then run_rotors (rotor_outcome - delta) tl (number + 1)
					else run_rotors (26+(rotor_outcome - delta)) tl (number+1) in
	let buf = run_rotors letter list_rotors 1 in
	run_inv buf (List.rev list_rotors) 1

(* This function sets the initial position of rotors
it should be fed with a list of rotors and int list (eg. [1;1;1] for AAA)*)
let grundstellung (rotors: rotor list) (setting: int list) : rotor list =
	let rec click_n_times (r:rotor) (curr: int) : rotor =
	if r.current = curr then r
	else  click_n_times (make_rotor (clicker r.comb) (clicker r.alpha) (r.notch) (check_position (r.current+1))) curr in
	let rec iter_rotors (rot: rotor list) (sett: int list) : rotor list =
    match rot with
	|[] -> failwith "\nGrundstellung: Empty list"
	|[hd1] -> [hd1]
	|hd1::tl1 -> 
		match sett with
		|[] -> []
        |[hd2] -> (click_n_times hd1 hd2)::iter_rotors tl1 []
        |hd2::tl2 -> (click_n_times hd1 hd2):: iter_rotors tl1 tl2 in
	iter_rotors rotors (List.rev setting)


(* This function changes the initial position of rings in rotors*)
(* it should be fed with a list of rotors and int list (eg. [1;1;1] for AAA) *)
let ringstellung (rotors: rotor list) (setting: int list): rotor list =
    let rec click_n_times (l: int list) (curr: int) (n: int): int list =
    if curr = n then l
    else  click_n_times (counter_clicker l) (curr+1) n in
    let rec iter_rotors (rot: rotor list) (sett: int list) : rotor list =
    match rot with
    |[] -> failwith "\nGrundstellung: Empty list"
    |[hd1] -> [hd1]
    |hd1::tl1 -> 
        match sett with
        |[] -> []
        |[hd2] -> (make_rotor (List.map (click_n_times hd1.comb 1 hd2) ~f:(fun x -> check_position (x+(hd2-1)))) hd1.alpha hd1.notch hd1.current):: iter_rotors tl1 []
        |hd2::tl2 -> (make_rotor (List.map (click_n_times hd1.comb 1 hd2) ~f:(fun x -> check_position (x+hd2-1))) hd1.alpha hd1.notch hd1.current):: iter_rotors tl1 tl2 in
    iter_rotors rotors (List.rev setting)

let string_enigma (input: bytes) (rotors: rotor list) : int list =
	let rec iterate (inp: int list) (r: rotor list) : int list =
	let new_r = check_rotors r 1 in
	match inp with
	|[] -> []
	|[hd] -> [letter_enigma hd new_r]
	|hd::tl -> (letter_enigma hd new_r) :: (iterate tl new_r) in
	iterate (encode_string input) rotors;;

let enigma (message: bytes) (rotors: rotor list) (grund: bytes) (ringstel: bytes) : bytes =
    (* VALIDATE INPUT *)
    if String.length grund <> 3 
    then failwith "\nEnigma: Initial position of rotors (Grundstellung) is invalid. Provide 3 letters."
    else if String.length ringstel <> 3
    then failwith "\nEnigma: Initial position of rings (Ringstellung) is invalid. Provide 3 letters."
    else
    (* INITIALIZATION: encode strings into valid format (int list) *)
    let ringstel_int = encode_string ringstel in
    let grund_int = encode_string grund in
    (* INITIALIZATION: Run Ringstellung and Grundstellung *)
    let new_rotors = grundstellung (ringstellung rotors ringstel_int) grund_int in
    (* ENCRYPT THE MESSAGE *)
    let encrypted_message = string_enigma message new_rotors in
    (* RETURN BACK TO STRING *)
    decode_string encrypted_message

(* FOR TESTING PURPOSES *)
let r22a = make_rotor r2 alphabet n2 1;;
let r22b = make_rotor (clicker r2) (clicker alphabet) n2 2 ;;
let r22c = make_rotor (clicker (clicker r2)) (clicker (clicker alphabet))n2 3;;
let r22d = make_rotor (clicker (clicker (clicker r2))) (clicker (clicker (clicker alphabet)))n2 4;;
let r33a = make_rotor r3 alphabet n3 1;;
let r33b = make_rotor (clicker r3) (clicker alphabet) n3 2;;
let r33c = make_rotor (clicker (clicker r3)) (clicker (clicker alphabet)) n3 3;;
let r33d = make_rotor (clicker (clicker (clicker r3))) (clicker (clicker (clicker alphabet))) n3 4;;
let r11a = make_rotor r1 alphabet n1 1;;
let refl = make_rotor u2 alphabet [] 1;;
let list = [r33a; r22a; r11a; refl];;


let test1 = enigma "One morning when Gregor Samsa woke from troubled dreams he found 
himself transformed in his bed into a horrible vermin  He lay on his armour like back 
and if he lifted his head a little he could see his brown belly  
slightly domed and divided by a" list "AUX" "HLP";;
let _ = assert(test1 = "PBRASKYDTYHGMSNAVKGVRYDMLRECQYOUHNBJGTCYVXCPLVAZPMSPMBRQWZIYPJXXSZXRZZNHJPOAOKIVQFPQODLTZDNVVSGMDHRIHVFCXWBYBRHEBTTEPXDQKNXCVCFNGRHRABBSPJNNDQIVJHAOZRNMBDVDQDWIJWPWOEKQGZNCCEJANFEZXGPGRBBOAJLWPVMOLCZ");;

(* This is a German message sent on Saturday, 7 February 1942 form 
 German submarine U-106. It translates to:
On the east coast of North America sank German submarines six enemy merchant 
vessels with a total of 30,000 GRT. Thereby particularly distinguished itself 
the submarine of Captain Lieutenant Rasch.*)
let test2 = enigma "An der Ostkuste Nordamerikas versenkten 
deutsche Unterseeboote sechs feindliche Handelsschiffe mit zusammen 
30 000 BRT. Dabei zeichnete sich das Unterseeboot des Kapitanleutnants 
Rasch besonders aus." list "MJJ" "KLD";;
let _ = assert(test2 = "MZQDJFUIPGQAXALLVSEPMVIUNQIXNTZEIAFAMGQJSJNFDSQWCAWTPNMGOVJSWTJBLSIRXJHKQIWHIACUNMIEOOVYYXUCPOSMKZMXIQVOJKNNKGZQRPHZWSTZDYRDLLIYVWVRWMFYFLDNQFWMPBHDGVWWKDZYYVJAQWUCLVJC");;

(* And now checking whether coding back actually works*)
let test3 = enigma "MZQDJFUIPGQAXALLVSEPMVIUNQIXNTZEIAFAMGQJSJNFDSQWCAWTPNMGOVJSWTJBLSIRXJHKQIWHIACUNMIEOOVYYXUCPOSMKZMXIQVOJKNNKGZQRPHZWSTZDYRDLLIYVWVRWMFYFLDNQFWMPBHDGVWWKDZYYVJAQWUCLVJC"
list "MJJ" "KLD";;
let _ = assert(test3 = "ANDEROSTKUSTENORDAMERIKASVERSENKTENDEUTSCHEUNTERSEEBOOTESECHSFEINDLICHEHANDELSSCHIFFEMITZUSAMMENBRTDABEIZEICHNETESICHDASUNTERSEEBOOTDESKAPITANLEUTNANTSRASCHBESONDERSAUS");;
