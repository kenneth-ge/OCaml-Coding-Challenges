let rec repeat f = function
| 0 -> ()
| n -> f (); repeat f (n - 1)

let next_int () = 
	Scanf.scanf " %d" (fun x -> x)

let rec read_list = function
	| 0 -> []
	| n -> (next_int ())::read_list (n - 1)

let rec print_list = function
	| [] -> ()
	| a::l -> print_int a; print_string " "; print_list l

let solve () = 
	let n = next_int () and k = next_int () in
	let l = List.sort (fun x y -> x - y) (read_list n) in
	let rec f ch acc = function
	| [] -> ([], 0)
	| a::l -> if a = ch then (f ch (acc + 1) l) else (
		let (x, y) = (f a 1 l) in
		((if acc >= k then a::x else x), (if acc >= k then 1 else 0) + y)
	)
	in
	let (nums, num) = f (List.hd l) 1 (List.tl l) in
	if num = 0 then print_string "-1\n" else
	print_list nums; print_newline ();;

let () = 
	let t = read_int () in
	repeat (solve) t