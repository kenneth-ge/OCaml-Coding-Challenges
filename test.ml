let rec repeat f = function
| 0 -> ()
| n -> f (); repeat f (n - 1)
in
repeat (fun () -> print_string "Hi\n") 10