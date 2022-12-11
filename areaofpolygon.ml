let rec first_of_list = function
    | [] -> failwith "empty"
    | e::l -> e

let calc_val a b = 
    (fst a * snd b) - (snd a * fst b)

let next_int () = 
    Scanf.scanf " %d" (fun a -> a)

let read_points n = 
    let rec f = function
    | 0 -> []
    | n -> (next_int (), next_int ())::(f (n - 1)) in
    f n

let () = 
    let n = read_int () in
    let points = read_points n in
    let rec shoelace_theorem acc = function
    | [] -> failwith "empty"
    | [e] -> (e, acc)
    | a::(b::l as rest) -> (shoelace_theorem (acc + (calc_val a b)) rest)
    in
    let (p, a) = shoelace_theorem 0 points in
    print_float (float_of_int (a + (calc_val p (first_of_list points))) /. 2.)