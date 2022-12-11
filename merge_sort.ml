let rec quick_sort = function
| [] -> []
| [e] -> [e]
| (e::l as acc) -> List.append (quick_sort (List.filter (fun x -> x <= e) acc))
	(quick_sort (List.filter (fun x -> x > e) acc))

