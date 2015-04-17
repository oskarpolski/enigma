open Printf
open Decrypt
open Enigma4


   (* UPDATE ROTORS *)                    
let r1 = encode_string "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
let r2 = encode_string "AJDKSIRUXBLHWTMCQGZNPYFVOE"
let r3 = encode_string "BDFHJLCPRTXVZNYEIWGAKMUSQO"
let r4 = encode_string "ESOVPZJAYQUIRHXLNFTGKDCMWB"
let r5 = encode_string "VZBRGITYUPSDNHLXAWMJQOFECK"
let r6 = encode_string "JPGVOUMFYQBENHZRDKASXLICTW"
let r7 = encode_string "NZJHGRCXMYSWBOUFAIVLPEKQDT"
let r8 = encode_string "FKQHTLXOCBJSPDZRAMEWNIUYGV"

let rotors = [r1;r2;r3;r4;r5;r6;r7;r8]

let n1 = encode_string "Q"
let n2 = encode_string "E"
let n3 = encode_string "V"
let n4 = encode_string "J"
let n5 = encode_string "Z"
let n6 = encode_string "ZM"
let n7 = encode_string "ZM"
let n8 = encode_string "ZM"

(* REFLECTOR THE SAME EVERYWHERE*)
let u2 = encode_string "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let alphabet = encode_string "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"

let all_letters (input: string) : bool =
if (String.contains input '0') ||
   (String.contains input '1') ||
   (String.contains input '2') || 
   (String.contains input '3') || 
   (String.contains input '4') || 
   (String.contains input '5') || 
   (String.contains input '6') || 
   (String.contains input '7') || 
   (String.contains input '8') || 
   (String.contains input '9')
then false else true
;;

let all_nums (input: string) : bool =
  let input = String.lowercase input in
  if (String.contains input 'a') ||
     (String.contains input 'b') ||
     (String.contains input 'c') || 
     (String.contains input 'd') || 
     (String.contains input 'e') || 
     (String.contains input 'f') || 
     (String.contains input 'g') || 
     (String.contains input 'h') || 
     (String.contains input 'i') || 
     (String.contains input 'j') ||
     (String.contains input 'k') ||
     (String.contains input 'l') || 
     (String.contains input 'm') || 
     (String.contains input 'n') || 
     (String.contains input 'o') || 
     (String.contains input 'p') || 
     (String.contains input 'q') || 
     (String.contains input 'r') ||
     (String.contains input 's') ||
     (String.contains input 't') || 
     (String.contains input 'u') || 
     (String.contains input 'v') || 
     (String.contains input 'w') || 
     (String.contains input 'x') || 
     (String.contains input 'y') || 
     (String.contains input 'z')
then false else true
;;

let rec string_to_rotor_list (s: string) (n:  int) : rotor list =
  match s.[n] with
  |' ' -> []
  | a -> let x = char_to_int a in
	 match x with
	 | 1 -> (Enigma4.make_rotor r1 alphabet n1 1) :: string_to_rotor_list s (n + 1)
	 | 2 -> (Enigma4.make_rotor r2 alphabet n2 1) :: string_to_rotor_list s (n + 1)
	 | 3 -> (Enigma4.make_rotor r3 alphabet n3 1) :: string_to_rotor_list s (n + 1)
	 | 4 -> (Enigma4.make_rotor r4 alphabet n4 1) :: string_to_rotor_list s (n + 1)
	 | 5 -> (Enigma4.make_rotor r5 alphabet n5 1) :: string_to_rotor_list s (n + 1)
	 | 6 -> (Enigma4.make_rotor r6 alphabet n6 1) :: string_to_rotor_list s (n + 1)
	 | 7 -> (Enigma4.make_rotor r7 alphabet n7 1) :: string_to_rotor_list s (n + 1)
	 | 8 -> (Enigma4.make_rotor r8 alphabet n8 1) :: string_to_rotor_list s (n + 1)
	 | _ -> failwith "Rotor numbers must be between 1 and 8"
;;


let () =
    if (all_letters Sys.argv.(1) = true) then (let MSG = Sys.argv.(1)) else (failwith "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")
    if Sys.argv.(2) = "" then break_enigma MSG

    if (String.length (Sys.argv.(2)) = 3) && (all_nums Sys.argv.(3) = true) then (let rotororder = (string_to_rotor_list Sys.argv.(3) 0)) 
    else (failwith "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")

    if (String.length (Sys.argv.(3)) = 3) && (all_letters Sys.argv.(3) = true) then (let rotorpos = Sys.argv.(3)) 
    else (failwith "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")

    if (String.length (Sys.argv.(4)) = 3) && (all_letters Sys.argv.(4) = true) then (let rotorpos2 = Sys.argv.(4)) 
    else (failwith "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")


    Enigma4.enigma MSG ((List.rev rotororder)@reflector) rotorpos rotorpos2
;;





(* 
    if (all_letters Sys.argv.(1) = true) then (let MSG = Sys.argv.(1)) else (printf "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")
    if Sys.argv.(2) = "" then break_enigma MSG
    if (String.length (Sys.argv.(2)) = 3) && (all_nums Sys.argv.(3) = true) then (let rotororder = (string_to_rotor_list Sys.argv.(3) 0)) 
    else (printf "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")
    if (String.length (Sys.argv.(3)) = 3) && (all_letters Sys.argv.(3) = true) then (let rotorpos = Sys.argv.(3)) 
    else (printf "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")
    if (String.length (Sys.argv.(4)) = 3) && (all_letters Sys.argv.(4) = true) then (let rotorpos2 = Sys.argv.(4)) 
    else (printf "Please provide a message, a three number rotor order, a three letter rotor position, and a three letter ring positions\n")
*)







  (* Okay, so if there is only one argument we want to run break enigma so you are simply gonna call a function
  break_enigma from Decrypt.ml*)
  (**)

  (* NOW CHECK EACH OF THE ARGUMENTS:
      MSG HAS TO BE A STRING
      ROTOR HAS TO BE THREE NUMBERS
      ROTORPOS HAS TO BE THREE LETTERS
      ROTORPOS2 HAS TO BE THREE LETTERS
*)

  (* NOW YOUR FUNCTION building a list of three rotors*)
  (*1. You take a string of three numbers (like 647)
    2. You have to build a list that we use later on for encryption
    3. To do this you have to use make rotor function (from Enigma4)
          make_rotor (combination: int list) (alphab: int list) (inotch: int list) (pos: int): rotor
  and build a list of rotors using this function in a following order (given the example 647)
    then
    let rotorlist = [rotor 7, rotor 4, rotor 6, reflector] -> IMPORTANT THEY ARE IN REVERSE ORDER
    in order to make a rotor call the function make rotor. for example rotor 7 will be
    let rotor7 = make_rotor r7 alphabet n7 1
    OK?
  
    its up to you how you do it
        *)


  (* if all of above are true then run function from Enigma4
    let encoded_message = enigma MSG (encode_string MSG) rotorlist rotorpos rotorpos2
  let _ = Printf.printf encoded_message*)



  done;;
