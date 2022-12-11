let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let process p1 p2 = 
    let inc = List.map2 (fun a b -> (a = b)) p1 p2 in
    let rec calc pref l1 l2 = 
    if not pref then ([], l1, l2) else (
        match l1, l2 with
        | a::l, b::l -> if a = b then 
        | [], b::l ->
        | a::l, [] ->
        | [], [] -> 
    )

let () = 
    let to_string s = string_of_chars s (List.length s) in
    let p1 = explode (read_line ()) in
    let p2 = explode (read_line ()) in
    let (x, y, z) = process p1 p2