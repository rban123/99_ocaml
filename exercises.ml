
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

(* 7. Flatten a List *)
type 'a node =
  | One of 'a 
  | Many of 'a node list


let flatten_list lst =
  let rec aux acc = function
    | [] -> acc
    | One h :: t -> aux (h :: acc) t
    | Many h :: t ->  aux (aux acc h) t
  in List.rev @@ aux [] lst


let lst = [3; 4; 5; 1; 20; 9] in
let nested_lst = [One "a"; Many [One "b"; Many [One "c"; One "d"]]; One "e"];;
