let size = 10000000;;
let rList n =
  let rec build_list n out =
    if n == 0 then out
    else build_list (n - 1) ((Random.int 10000) :: out)
  in build_list n [];;

let q = Array.of_list (rList size);;
let r = Array.of_list (rList 40);;
Array.fast_sort Int.compare r;;

let rec bin_search ar q idx roof =
  let cval = Array.get ar idx in
  if cval == q then true
  else if roof == idx + 1 then false
  else if cval > q then bin_search ar q (idx lsr 1) idx
  else bin_search ar q ((idx + roof) lsr 1) roof;;

let rec num_differencesAr listA listB n idx =
  if idx == Array.length listA then n
  else
    let a = Array.get listA idx in
    if bin_search listB a (40 lsr 1) 40
    then num_differencesAr listA listB n (idx + 1)
    else num_differencesAr listA listB (n + 1) (idx + 1);;

print_int (num_differencesAr q r 0 0);;
