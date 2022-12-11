let next_line_int = fun () -> 
  int_of_string(String.trim (read_line ()));;

let read_pair = fun () -> 
  let (c, d) = Scanf.scanf " %d %d " (fun a b -> (a, b)) in
  (c, d)

let f n arr = 
  let rec replicate_n n x = match n with 
    | 0 -> []
    | n -> x :: replicate_n (n - 1) x
  in
  let unflattened = List.map (replicate_n n) arr in
  List.flatten unflattened;;

let read_input n = 
  let rec acc = function
    | 0 -> []
    | n -> read_pair () :: acc (n - 1)
    in
  acc n

let rec count f acc = function
  | [] -> acc
  | a::l -> count f (if (f a) then (acc + 1) else acc) l

let first (a, _) = a

let rec sum = function 
  | [] -> 0
  | a::l -> a + (sum l)

let main () = 
  let n = next_line_int () in
  let values = read_input n in
  let count_valid value arr = if (count (fun (x,_) -> x == value) 0 values) == 1 then 1 else 0 in
  let total = sum (List.map (fun x -> count_valid (first x) values) values) in
  if total == n then "YES" else "NO"

let rec test_case = function
  | 0 -> print_int 0; ()
  | n -> print_int n; print_string (main ()); test_case (n - 1);;

let t = next_line_int () in
test_case t