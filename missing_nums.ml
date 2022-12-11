let next_int () = Scanf.scanf " %d" (fun x -> x);;

let rec read_list = function
    | 0 -> []
    | n -> next_int ()::(read_list (n - 1));;

let rec print_list = function
    | [] -> print_newline ()
    | a::l -> print_int a; print_string " "; print_list l

let rem_from_right lst =
  let rec is_member n mlst =
    match mlst with
    | [] -> false
    | h::tl ->
        begin
          if h=n then true
          else is_member n tl
        end
  in
  let rec loop lbuf =
    match lbuf with
    | [] -> []
    | h::tl ->
        begin
        let rbuf = loop tl
        in
          if is_member h rbuf then rbuf
          else h::rbuf
        end
  in
  loop lst

let () = 
    let n = next_int () in
    let a = List.sort compare (read_list n) in
    let m = next_int () in
    let b = List.sort compare (read_list m) in
    let rec f x y = 
        match x, y with
        | [], [] -> []
        | [], l -> l
        | _, [] -> failwith "bad input"
        | a::l1, b::l2 -> if a = b then f l1 l2 else b::(f x l2) in
    let sorted_ans = rem_from_right (List.sort compare (f a b)) in
    print_list sorted_ans
