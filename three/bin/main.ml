let rec find_n n = function 
        | [] -> None
        | h :: t -> if n = 1 then Some h else find_n (n - 1) t 
;;


let list = [5;10;15;20] in 
let el = find_n 2 list in 
match el with 
| Some a -> print_int a
| None -> print_endline "none"
;;
