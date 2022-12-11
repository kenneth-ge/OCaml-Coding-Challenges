let next_int () = 
    Scanf.scanf " %d" (fun a -> a)

let next_float () = 
    Scanf.scanf " %f" (fun a -> a)

let read_points n = 
    let rec read = function
    | 0 -> []
    | n -> (next_float (), next_float ()) :: read (n - 1) in
    read n

let rec consec f = function
    | [] -> 0.
    | [e] -> 0.
    | a::b::l -> (f a b) +. (consec f (b::l))

let dist (a, b) (c, d) = 
    ((a -. c) *. (a -. c) +. (b -. d) *. (b -. d)) ** (1./.2.)

let wraparound l = 
    let first_item = function
    | [] -> failwith "too bad"
    | e::_ -> e
    in
    l @ [first_item l] 

let perim points = 
    consec dist (wraparound points)

let () = 
    let n = next_int () in
    let points = read_points n in
    print_float (perim points)