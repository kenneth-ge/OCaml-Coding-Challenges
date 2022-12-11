let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec print_list = function
	| [] -> ()
	| e::f -> print_char e; print_list f

let rec repeat f = function
	| 0 -> ()
	| n -> f (); repeat f (n - 1)

let swap a = 
	let rec f = function
	| a::b::l -> b::a::(f l)
	| a::_ -> [a]
	| [] -> [] in
	f a

let () = 
	let k = read_int () in
	let calculate = fun () -> (
		let a = explode (read_line ()) in
		print_list (swap a); print_newline ()) in
	repeat calculate k