let next_int () = 
    Scanf.scanf " %d" (fun a -> a)

let next_float () = 
    Scanf.scanf " %f" (fun a -> a)

let rec gcd a = function
    | 0 -> a
    | b -> gcd b (a mod b)

let rec prime_factor num factor =
    if num == 1 then 1 else if factor * factor > num then 2 else
        let rec repeat num factor = 
            if (num mod factor == 0) then 
                let (a, b) = (repeat (num / factor) factor) in
                (a + 1, b)
            else
                (0, num)
        in
        let (times, remainder) = repeat num factor in
        (times + 1) * (prime_factor remainder (factor + 1))

let solve () = 
    let l = next_int () in
    let m = next_int () in
    let div = gcd l m in
    print_int (prime_factor div 2); print_string "\n"

let rec recurse = function
    | 0 -> ()
    | t -> solve (); recurse (t - 1)

let () = 
    let t = next_int () in
    recurse t