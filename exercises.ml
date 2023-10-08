
(* 1. Tail of a List *)
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last t

(* 2. Last Two Elements of a List *)
let rec last_two = function
  | [] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two t 

(* 3. N'th Element of a List *)
let nth n lst = 
  let rec nth_helper n acc = function 
    | [] -> None
    | h :: t -> if n = acc then Some h else 
      nth_helper n (acc + 1) t
  in nth_helper n 0 lst

(* 4. Length of a List *)
let length lst = 
  let rec length_helper acc = function
    | [] -> acc
    | _ :: t -> length_helper (acc + 1) t
  in length_helper 0 lst

(* 5. Reverse a List *)
let rev lst =
  let rec rev_helper acc = function
    | [] -> acc
    | h :: t -> rev_helper (h :: acc) t
  in rev_helper [] lst
   

(* 6. Palindrome *)
let is_palindrome lst = 
  lst = rev lst



