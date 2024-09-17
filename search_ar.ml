let size = 10000000;;

let rList n =
  let rec build_list n out =
    if n == 0 then out
    else build_list (n - 1) ((Random.int 10000) :: out)
  in build_list n [];;

let q = Array.of_list (rList size);;
let r = Array.of_list (rList 40);;

let rec num_differencesAr listA listB n idx =
  if idx == Array.length listA then n
  else
    let a = Array.get listA idx in
    let comp z = a == z in
    match Array.find_opt comp listB with
    | Some _ -> num_differencesAr listA listB n (idx + 1)
    | None -> num_differencesAr listA listB (n + 1) (idx + 1);;

print_int (num_differencesAr q r 0 0);;
