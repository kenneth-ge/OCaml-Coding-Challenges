let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let c2s c = String.make 1 c

let handle_int count = 
    if count = 1 then () else print_int count

let compress list = 
    let rec inner old count = function
    | [] -> print_string (c2s old); handle_int count
    | e::l -> if (old = e) then (inner old (count + 1) l) else (print_string (c2s old); handle_int count; inner e 1 l) in
    let a::l = list in
    inner a 1 l

let () = 
    let line = read_line () in
    let list = explode line in
    compress list
