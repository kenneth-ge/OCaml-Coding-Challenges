let rec print_list = function 
	[] -> ()
	| e::l -> print_int e ; print_string " " ; print_list l

let print_and_ret x = 
	print_int x; print_newline (); x

let rec pascal r = 
	if r == 1 then [((print_and_ret 1); 1)] else
	let prev_row = pascal (r - 1) in
	let rec new_row = function
	| a::(b::_ as tl) -> (a + b)::(new_row tl) 
	| e -> e in
	let l = 1::(new_row prev_row) in
	let k = print_list l; print_newline () in
	l

let () = 
	let k = read_int () in
	pascal k;
	()