type 'a rle = 
        | One of 'a
        | Many of int * 'a
;;

(** 
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
**)


let encode list = 
let rec create_sequence count acc = function 
        | [] -> acc 
        | [single] -> (if count = 0 then One single else Many (count + 1, single)) :: acc
        | f :: (s :: _ as t) -> if f = s then create_sequence (count + 1) acc t else 
                create_sequence 0 ((if count = 0 then One f else Many (count + 1, f)) :: acc) t
in 
create_sequence 0 [] list;;

let result = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];; 

let _ =
        let print_element = function 
                | Many (count, element) -> "Count: " ^ (string_of_int count) ^ " Element: " ^ element ^ "\n"  
                | One element -> "Single element: " ^ element 
        in
        List.iter (fun el -> print_endline (print_element el)) result;; 
      

