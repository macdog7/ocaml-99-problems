type 'a node =
        | One of 'a
        | Many of 'a node list;;

let flatten list =
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many l :: t -> aux (aux acc l) t in
    List.rev (aux [] list);;

let result = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;

let rec print_arr arr = 
        match arr with 
        | [] -> print_endline ""
        | first :: rest -> print_endline first; print_arr rest
;;

let _ = print_arr result;;
