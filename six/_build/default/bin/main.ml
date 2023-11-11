let is_palindrome list = list = List.rev list;;

let r = is_palindrome ["x";"a";"b";"a";"x"] in
print_endline (string_of_bool r)
;;
