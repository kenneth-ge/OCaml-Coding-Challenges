let fib n = 
    let a = ref 0 and b = ref 1 and c = ref 1 in
    let i = ref 0 in
    for i = 0 to n do
        c := !a + !b;
        a := !b;
        b := !c
    done;
    c

let () = 
    let n = read_int () in
    print_int (fib n)