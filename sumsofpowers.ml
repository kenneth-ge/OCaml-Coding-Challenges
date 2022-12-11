let rec firstNIntegers = function
| 0 -> []
| n -> n::firstNIntegers (n - 1)
in
let rec power n x = 
	match n with
	| 1 -> x
	| _ -> x * (power (n - 1) x)
in
let rec count_ways x l potential =
	if x == 0 then 1
	else if x < 0 || potential < x then 0
	else
		let head = List.hd l in
		let tail = List.tl l in
		let new_potential = potential - head in
		(count_ways (x - head) tail new_potential) + (count_ways x tail new_potential)
in
let target = read_int () in
let pw = read_int () in
let filtered_entry = (List.filter (fun y -> y <= target) (List.map (power pw) (firstNIntegers target))) in
let pot = List.fold_left (+) 0 filtered_entry in
print_int (count_ways target filtered_entry pot) ; print_newline ();;