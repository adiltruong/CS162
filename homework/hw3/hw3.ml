(* return a list that contains the integers i through j inclusive *)
let rec interval i j =
(* to be written *)
    if i > j
      then []
    else
      i :: interval(i+1) j;;

(* Use tail-recursion to compute the number of elements in the list *)
let rec length list =
  match list with
  | [] -> 0
  | h::t -> 1 + (length t);;
(* to be written *)

(* Eliminate consecutive duplicates of list elements. *)
let rec compress list =
  match list with
  | [] -> []
  | [a] -> [a]
  | h :: g :: t -> if h = g
                   then compress (g::t)
                   else [h] @ (compress (g::t));;


(* Check if n is a prime number *)
 let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2;;

(* Goldbach's conjecture *)
let goldbach n =
let rec gold d =
    if is_prime d && is_prime (n - d)
      then (d, n-d)
    else gold (d+1)
  in gold 2;;
(* to be written *)

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

(*Symmetric binary trees.*)

let rec help_sym = function
  | Empty, Empty -> true (*base case*)
  | _, Empty | Empty, _ -> false
  | Node (_, lf, rf), Node (_, ls, rs) -> help_sym(lf, rs) && help_sym(ls, rf);;

let is_symmetric t =
  match t with
  | Empty -> true
  | Node (_, l, r) -> help_sym(l,r);;
(* to be written *)


(* Eight queens problem. *)
let queens_positions n = [[1]];;
(* to be written *)
