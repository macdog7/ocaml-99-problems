let length list = 
        let rec count n = function
                | [] -> n
                | _ :: b -> count (n + 1) b
        in 
        count 0 list;;


let test = length [50;20;30]
in print_endline (string_of_int test)
;;


