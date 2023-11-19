let rec repeat_n element count = if count < 1 then element :: [] else element :: (repeat_n element (count - 1))
;;

let rec replicate list n = 
        match list with 
        | [] -> [] 
        | hd :: tail -> (repeat_n hd (n - 1)) @ replicate tail n
;;

