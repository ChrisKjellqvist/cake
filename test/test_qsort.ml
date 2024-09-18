let ar_size = 10000;;
let n_ars = 1000;;

let is_sorted_array arr =
  let arlen = Array.length arr in
  let rec help idx =
    if idx + 1 >= arlen then true
    else if (Array.get arr idx) > (Array.get arr idx+1) then false
    else help (idx + 1)
  in help 0;;

let genRandomList n = List.init n (fun _ -> Random.int(100000));;
let genRandomArr n = Array.of_list (genRandomList n);;
let rec do_trials n =
  if n == 0 then true
  else
    let arr = genRandomArr ar_size in
    let () = Quicksort.generic_qsort (Array.get arr) (Array.set arr) (Int.compare) 0 ar_size
    in let success = is_sorted_array arr
      in if success then do_trials (n-1)
      else failwith "";;

let rec do_trials_stdlib n =
  if n == 0 then true
  else
    let arr = genRandomArr ar_size in
    let () = Array.fast_sort Int.compare  arr in
    let success = is_sorted_array arr
      in if success then do_trials_stdlib (n-1)
      else failwith "";;

let tput =
  let start = Unix.gettimeofday()
  in let _ = do_trials n_ars
    in let stop = Unix.gettimeofday()
      in (stop -. start);;

let tput_std =
  let start = Unix.gettimeofday()
  in let _ = do_trials_stdlib n_ars
    in let stop = Unix.gettimeofday()
      in (stop -. start);;

Printf.printf "Stdlib: %0.2fs\t Qsort: %0.2fs\n" tput_std tput;;
