let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

let rec remove_dup arr = function
    | [] -> []
    | e::l -> if (Array.get arr e) then (remove_dup arr l) else (
        arr.(e) <- true; 
        e::(remove_dup arr l)
    )

let print_list l = 
    let chars = List.map (fun x -> Char.chr (x + Char.code 'a')) l in
    let escaped = List.map Char.escaped chars in
    List.map (fun x -> print_string x) escaped

let () = 
    let str = read_line () in
    let ints = List.map (fun x -> x - (Char.code 'a'))(List.map (Char.code) (explode str)) in
    let arr = Array.make 27 false in
    print_list (remove_dup arr ints); print_newline ()