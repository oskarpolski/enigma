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

  (*TO ZABIE*)

  let () =
  (* Okay, so if there is only one argument we want to run break enigma so you are simply gonna call a function
  break_enigma from Decrypt.ml*)
  (**)
  if (Sys.argv.(1) = "") then break_enigma Sys.argv.(1) (*(printf "Please provide a message, rotor order, rotor position, and ring positions\n") *)else
  let MSG = Sys.argv.(1)
  if (Sys.argv.(2) = []) then (printf "Please provide a message, rotor order, rotor position, and ring positions\n") else
  let rotor = Sys.argv.(2)
  if (Sys.argv.(3) = "") then (printf "Please provide a message, rotor order, rotor position, and ring positions\n") else
  let rotorpos = Sys.argv.(3)
  if (Sys.argv.(4) = "") then (printf "Please provide a message, rotor order, rotor position, and ring positions\n") else
  let rotorpos2 = Sys.argv.(4)



  (* NOW CHECK EACH OF THE ARGUMENTS:
      MSG HAS TO BE A STRING
      ROTOR HAS TO BE THREE NUMBERS
      ROTORPOS HAS TO BE THREE LETTERS
      ROTORPOS2 HAS TO BE THREE LETTERS*)

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
  let _ = Printf.printf *)



  done;;