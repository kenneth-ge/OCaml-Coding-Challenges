let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec print_list = function
	| [] -> ()
	| e::f -> print_char e; print_list f

let mingle p q = 
	let rec calc acc p q = 
		match p, q with
		| (a::l),l2 -> calc (a::acc) l2 l
		| [],l -> List.flatten [l;acc]
	in
	calc [] p q

let () = 
	let p = explode (read_line ()) in
	let q = explode (read_line ()) in
	print_list (List.rev (mingle p q))