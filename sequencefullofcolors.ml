(* Sequence full of colors *)
let abs x = if x < 0 then -x else x

let rec repeat f = function
| 0 -> ()
| n -> f (); repeat f (n - 1)

let rec calc = fun () -> (
	let line = read_line () in
	let rec calc r g b y = (function
	| len when (len = String.length line) -> (r = g && b = y)
	| n -> (
		match (String.get line n) with
		| 'R' -> if abs (r - g + 1) <= 1 then calc (r + 1) g b y (n + 1) else false
		| 'G' -> if abs (r - g - 1) <= 1 then calc r (g + 1) b y (n + 1) else false
		| 'B' -> if abs (b - y + 1) <= 1 then calc r g (b + 1) y (n + 1) else false
		| 'Y' -> if abs (b - y - 1) <= 1 then calc r g b (y + 1) (n + 1) else false
		| e -> failwith (Core.Char.to_string e)
	)
	| _ -> failwith "?") in
	print_string (if (calc 0 0 0 0 0) then "True" else "False"); print_newline ())

let () = 
	let t = read_int () in
	repeat calc t