let rec fib a b = function
    | 0 -> 0
    | 1 -> 1
    | 2 -> 1
    | 3 -> a + b
    | n -> fib b (a + b) (n - 1)

let () = 
    let n = read_int () in
    print_int (fib 0 1 n)
