let next_int () = 
  Scanf.scanf " %d" (fun a -> a)

let rec repeat f = function
  | 0 -> ()
  | n -> f (); repeat f (n - 1)

let read_list n = 
    let rec internal = function
    | 0 -> []
    | n -> next_int ()::(internal (n - 1)) in
    List.rev (internal n)

let used = Hashtbl.create 100

let rec print_list = function
  | [] -> print_string "\n"
  | e::l -> (if not (Hashtbl.mem used e) then (print_string (string_of_int e); 
                                print_string " "; Hashtbl.add used e 1); print_list l)

let print_hashtbl ht = 
  Hashtbl.iter (fun x y -> Printf.printf "%d -> %d\n" x y) ht;;

let rec print_list2 = function
| [] -> print_newline ()
| e::l -> print_int e; print_string " "; print_list2 l

let print_bool b = print_string (if b then "true" else "false")

let solve () = 
  let n = (Hashtbl.clear used); next_int () in
  let k = next_int () in
  let list = read_list n in
  let my_hash = Hashtbl.create (n / 2) in
  let add_to_hashtbl n = (
    if not (Hashtbl.mem my_hash n) then Hashtbl.add my_hash n 1 else Hashtbl.replace my_hash n ((Hashtbl.find my_hash n) + 1)
  ) in
  let asdf = List.iter add_to_hashtbl list in
  let remaining = (List.filter (fun x -> (Hashtbl.find my_hash x) >= k) list) in
  if (List.length remaining <> 0) then (print_list remaining) else print_string "-1\n"

let () = 
  let t = read_int () in
  repeat solve t