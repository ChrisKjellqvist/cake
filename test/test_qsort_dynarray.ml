let ar_size = 1000;;
let n_ars = 1000;;

let is_sorted_array arr =
  let arlen = Dynarray.length arr in
  let rec help idx =
    if idx + 1 >= arlen then true
    else if (Dynarray.get arr idx) > (Dynarray.get arr idx+1) then false
    else help (idx + 1)
  in help 0;;

let genRandomList n = List.init n (fun _ -> Random.int(100000));;
let genRandomArr n = Dynarray.of_list (genRandomList n);;
let rec do_trials n =
  if n == 0 then true
  else
    let arr = genRandomArr ar_size in
    let () = Quicksort.generic_qsort (Dynarray.get arr) (Dynarray.set arr) (Int.compare) 0 ar_size
    in let success = is_sorted_array arr
      in if success then do_trials (n-1)
      else failwith "";;

let tput =
  let start = Unix.gettimeofday()
  in let _ = do_trials n_ars
    in let stop = Unix.gettimeofday()
      in (stop -. start);;

Printf.printf "Qsort: %0.2fs\n" tput;;
