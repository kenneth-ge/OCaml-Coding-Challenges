let rec repeat f = function
    | 0 -> ()
    | n -> f (); repeat f (n - 1)

let arr = Array.make 100001 (-1)

let num n = 
    let rec num diff = function
    | 1 -> Array.set arr 1 1; 1
    | n -> (
        let k = diff + num (diff - 3) (n - 1) in
        Array.set arr n k; k
    ) in
    num (4 + 3 * (n - 2)) n

let calc () = 
    let n = read_int () in
    print_int (Array.get arr n); print_newline ()

let () = 
    let t = read_int () in
    num 100000;
    repeat calc t