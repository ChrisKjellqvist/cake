type my_type =
  | TypeA of int
  | TypeB of int * int;;
let hmap: (my_type, string) Hashtbl.t = Hashtbl.create 50;;

Hashtbl.add hmap (TypeB (3, 4)) "chris";;
Hashtbl.add hmap (TypeA 9) "Hana";;

Printf.printf "My name should be %s\n" (Hashtbl.find hmap (TypeB (3, 4)));;

let () = print_endline "Hello, World!";;

