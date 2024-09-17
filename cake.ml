type overlap_t =  NO_OVERLAP
  | OVERLAP_LEFTA 
  | OVERLAP_LEFTA_EDGE_RIGHTA
  | A_CONTAINS_B 
  | B_CONTAINS_A
  | OVERLAP_RIGHTA
  | EDGE_LEFTA_OVERLAP_RIGHTA
  | EQUALS
  | EDGE_LEFTA | EDGE_RIGHTA
  | OVERLAP_BOTH;;

let string_from_overlap q = match q with
  | NO_OVERLAP -> "NO_OVERLAP"
  | OVERLAP_LEFTA -> "OVERLAP_LEFTA"
  | OVERLAP_LEFTA_EDGE_RIGHTA -> "OVERLAP_LEFTA_EDGE_RIGHTA"
  | A_CONTAINS_B -> "A_CONTAINS_B"
  | B_CONTAINS_A -> "B_CONTAINS_A"
  | OVERLAP_RIGHTA -> "OVERLAP_RIGHTA"
  | EDGE_LEFTA_OVERLAP_RIGHTA -> "EDGE_LEFTA_OVERLAP_RIGHTA"
  | EQUALS -> "EQUALS"
  | EDGE_LEFTA -> "EDGE_LEFTA"
  | EDGE_RIGHTA -> "EDGE_RIGHTA"
  | OVERLAP_BOTH -> "OVERLAP_BOTH"

type is_finished_t =
  | DONE
  | REVERSED
  | NOT_DONE;; 
  
type arcPoint = {
  tup : int * int;
  eval : float
}
type arc_t = {
  start : arcPoint;
  fin : arcPoint;
  up : bool;
};;


type run_mode_t =
  | DO_PROBLEM
  | TEST;;

let do_problem a b c mode =
  let test_n = a * b * c * 10 in
  let sc = sqrt (float_of_int c)
  and total_degrees = float_of_int (a * b * c)
  and total_degrees_i = a * b * c in

  let create_arcPoint a b =
    let eval_pos a b = 
      let normalize q =
        if q < 0. then q +. Float.abs(Float.floor(q /. total_degrees)) *. total_degrees
        else if q > total_degrees then q -. Float.floor(q /. total_degrees) *. total_degrees 
        else q
      in
        let r = normalize(float_of_int a +. float_of_int b *. sc) in
        if r < 0. then failwith (String.concat " " [string_of_float r; string_of_float (float_of_int a +. float_of_int b *. sc)])
        else r in
    let modNorm q =
      if q >= total_degrees_i then q mod total_degrees_i
      else if q < 0 then total_degrees_i + (q mod total_degrees_i)
      else q in
    let fixA = modNorm a in
    { tup = (fixA, b); eval = eval_pos fixA b } in

  let arcPlus a b = create_arcPoint (fst a.tup + fst b.tup) (snd a.tup + snd b.tup) in 
  let arcMinus a b = create_arcPoint (fst a.tup - fst b.tup) (snd a.tup - snd b.tup) in 
  let eq {tup = a; eval = _} {tup = b; eval = _} = ((fst a) == (fst b)) && ((snd a) == (snd b)) in 
  let rec print_arcs lst = match lst with
    | a :: rst -> 
        let () = Printf.printf "%d: [ %0.2f(%d, %d), %0.2f(%d, %d) ] %s\n" (List.length rst) (a.start.eval) (fst a.start.tup) (snd a.start.tup) (a.fin.eval) (fst a.fin.tup) (snd a.fin.tup) (if a.up then "up" else "down")
        in print_arcs rst
    | [] -> ()
  and verbose = false in 
  (*in let comp_p a b =
    let apos = eval_pos a.start
    and bpos = eval_pos b.start
    in if apos > bpos then 1 else if apos == bpos then 0 else -1*)
  let cake_compare arcA arcB =
    let _ = if verbose then
      let _ = print_string "CALL TO COMPARE\n"
      and _ = print_arcs [arcA; arcB]
      in  print_string "END CALL\n" in
    if eq arcA.start arcA.fin then A_CONTAINS_B (* the entire circle is covered by A, so necessarily A contains B *)
    else if arcA.start.eval < arcA.fin.eval then (* no boundary-cross 0 *) (
      if arcB.start.eval < arcB.fin.eval then (* no boundary-cross 0 *) (
        if eq arcA.start arcB.start then (
          (* aaa
             bxxx *)
          if arcA.fin.eval < arcB.fin.eval then EDGE_LEFTA_OVERLAP_RIGHTA
            (* aaa
               bbbb *)
          else if eq arcA.fin arcB.fin then EQUALS
            (* aaa
               bbb *)
          else if arcA.fin.eval > arcB.fin.eval then EDGE_LEFTA
            (* aaa
               bb *)
          else failwith "Uncaught case 0"
        ) else if arcA.start.eval < arcB.start.eval then (
          (* aaa
              xxxx *)
          if eq arcA.fin arcB.fin then EDGE_RIGHTA
            (* aaa
                bb *)
          else if arcA.fin.eval < arcB.start.eval || eq arcA.fin arcB.start then NO_OVERLAP
            (* aaa
                  b *)
          else if arcB.fin.eval < arcA.fin.eval then A_CONTAINS_B
            (* aaa
                b *)
          else if arcB.fin.eval > arcA.fin.eval && arcB.start.eval < arcA.fin.eval then OVERLAP_RIGHTA
            (* aaa
                 bb *)
          else failwith "Uncaught case 1"
        ) else (
          (* aaa
            bxxxx *)
          if arcB.fin.eval < arcA.start.eval || eq arcB.fin arcA.start then NO_OVERLAP
            (*  aaa
               b *)
          else if eq arcA.fin arcB.fin then OVERLAP_LEFTA_EDGE_RIGHTA
            (*  aaa
               bbbb *)
          else if arcA.fin.eval < arcB.fin.eval then B_CONTAINS_A
            (*  aaa
              bbbbbb *)
          else if arcB.fin.eval > arcA.start.eval && arcB.fin.eval < arcA.fin.eval then OVERLAP_LEFTA
            (*  aaa
               bbb *)
          else failwith "Unncaught case 2"
        )
      ) else ( (* yes boundary cross of arcB across 0, but not arcA *)
        (* bb|bb
          xxx xxx... *)
        if eq arcA.start arcB.start then EDGE_LEFTA_OVERLAP_RIGHTA
          (*bb|bb
            aa *)
        else if arcA.start.eval > arcB.fin.eval || eq arcA.start arcB.fin then (
            (*bb|bb
             xxx   x..*)
          if arcA.fin.eval < arcB.start.eval || eq arcA.fin arcB.start then NO_OVERLAP
            (* bb|bb
              a     a.. *)
          else if arcA.fin.eval > arcB.start.eval && arcA.start.eval < arcB.start.eval then OVERLAP_RIGHTA
            (* bb|bb
              aa     *)
          else if arcA.fin.eval > arcB.start.eval && arcA.start.eval > arcB.start.eval then B_CONTAINS_A
            (* bb|bb
                a|   *)
          else failwith "Uncaught case 4"
        ) else if arcA.start.eval < arcB.fin.eval then (
          (* bb|bb
            xx   a...*)
          if eq arcA.fin arcB.fin then OVERLAP_LEFTA_EDGE_RIGHTA
            (* bb|bb
                  aa *)
          else if arcA.fin.eval < arcB.fin.eval then B_CONTAINS_A
            (* bb|bb
                  a   *)
          else if arcA.fin.eval < arcB.start.eval || eq arcA.fin arcB.start then OVERLAP_LEFTA
            (* bb|bb
              a    aa*)
          else if arcB.start.eval < arcA.fin.eval then OVERLAP_BOTH
            (* bb|bb
              aaa  aa... *)
          else failwith "Uncaught case 5"
        ) else failwith "Uncaught case 3"
      )
    ) else ( (* arcA overlaps 0 boundary*)
      if arcB.start.eval < arcB.fin.eval then ( (* arcB does not overlap 0 boundary *)
        (* aa|aa
          xx   xx *)
        if arcB.start.eval < arcA.fin.eval then (
            (* aa|aa
              xx  bbx *)
          if eq arcB.fin arcA.fin then EDGE_RIGHTA
            (* aa|aa
                  bb *)
          else if arcB.fin.eval < arcA.fin.eval then A_CONTAINS_B
            (* aa|aa
                  b *)
          else if arcB.fin.eval < arcA.start.eval || eq arcB.fin arcA.start then OVERLAP_RIGHTA
            (* aa|aa
              b   bbb..*)
          else if arcB.fin.eval > arcA.start.eval then OVERLAP_BOTH
          else failwith "Uncaught case 8"
        ) else if arcB.start.eval > arcA.fin.eval || eq arcA.fin arcB.start then (
          (* aa|aa
            xxx   x.. *)
          if eq arcB.start arcA.start then EDGE_LEFTA
            (* aa|aa
               b |  *)
          else if arcB.start.eval > arcA.start.eval then A_CONTAINS_B 
            (* aa|aa
                b|   *)
          else if arcB.fin.eval < arcA.start.eval || eq arcB.fin arcA.start then NO_OVERLAP
            (* aa|aa
              b     b..*)
          else if arcA.start.eval < arcB.fin.eval then OVERLAP_LEFTA
          else failwith "Uncaught case 9"
        ) else if eq arcA.start arcB.start then EDGE_LEFTA
          (* aa|aa
            eb *)
        else if arcB.start.eval > arcA.start.eval then A_CONTAINS_B
        else failwith "Uncaught case 7"
      ) else ( (* both arcA & arcB overlap 0 boundary *)
        (* aaa|aaa
          xxxx|xxxx *)
        if eq arcA.start arcB.start then (
          (* aaa|aaa
             bbb|bxxx *)
          if eq arcB.fin arcA.fin then EQUALS
            (* aaa|aaa
               bbb|bbb *)
          else if arcB.fin.eval < arcA.fin.eval then EDGE_LEFTA
            (* aaa|aaa
               bbb|b *)
          else if arcB.fin.eval > arcA.fin.eval then EDGE_LEFTA_OVERLAP_RIGHTA
            (* aaa|aaa
               bbb|bbbb *)
          else failwith "Uncaught case 11"
        ) else if arcB.start.eval > arcA.start.eval then (
          (* aaa|aaa
            xx b|bbxxx.. *)
          if eq arcA.fin arcB.fin then EDGE_RIGHTA
            (* aaa|aaa
                 b|bbb *)
          else if arcB.fin.eval < arcA.fin.eval then A_CONTAINS_B
            (* aaa|aaa
                 b|b    *)
          else if arcB.fin.eval > arcA.fin.eval && (arcB.fin.eval < arcA.start.eval || (eq arcB.fin arcA.start)) then OVERLAP_RIGHTA
            (* aaa|aaa
              b  b|bbbb.. *)
          else if arcB.fin.eval > arcA.fin.eval && arcB.fin.eval > arcA.start.eval then OVERLAP_BOTH
            (* aaa|aaa
              bb b|bbbb.. *)
          else failwith "Uncaught case 12"
        ) else if arcB.start.eval < arcA.start.eval && (arcB.start.eval > arcA.fin.eval || eq arcB.start arcA.fin)  then (
          (* aaa|aaa
            bbbb|bbxxx *)
          if eq arcA.fin arcB.fin then OVERLAP_LEFTA_EDGE_RIGHTA
            (* aaa|aaa
              bbbb|bbb *)
          else if arcB.fin.eval < arcA.fin.eval then OVERLAP_LEFTA
            (* aaa|aaa
              bbbb|bb *)
          else if arcB.fin.eval > arcA.fin.eval then B_CONTAINS_A
            (* aaa|aaa
              bbbb|bbbb *)
          else failwith "Unncaught case 13"
        ) else if arcB.start.eval < arcA.start.eval && (arcB.start.eval < arcA.fin.eval) then OVERLAP_BOTH
         (* aaa  aaa|aaa...
             bbbbbbb|bbb... *) 
        else failwith "Uncaught case 10"
      )
    ) in
  let print_comp_eval flip slice c =
    Printf.printf "flip (%f, %f) overlaps slice (%f, %f) with %s\n" (flip.start.eval) (flip.fin.eval) (slice.start.eval) (slice.fin.eval) (string_from_overlap c) in

  let areArcsAdjacent arcA arcB = arcA.up == arcB.up && (eq arcA.start arcB.fin || eq arcA.fin arcB.start) in

  let mergeAdjacentArcs arcA arcB =
    if eq arcA.start arcB.fin then { arcB with fin = arcA.fin }
    else { arcA with fin = arcB.fin } in 

  let simplifyAllSlices allSlicesUnsorted =
    let comp_slice a b = Float.compare (a.start.eval) (b.start.eval) in
    let allSlices = List.sort comp_slice allSlicesUnsorted in 
    let rec simplifySlicesHelp headSlice slices outputSlices =
      match slices with
        | hSlice :: rst ->
            if areArcsAdjacent headSlice hSlice
            then simplifySlicesHelp (mergeAdjacentArcs headSlice hSlice) rst outputSlices
            else simplifySlicesHelp hSlice rst (headSlice :: outputSlices)
        | [] -> 
            match List.rev outputSlices with
            | first :: rst ->
              if areArcsAdjacent first headSlice
              then (mergeAdjacentArcs first headSlice) :: rst
              else headSlice :: outputSlices
            | [] -> [headSlice]
    in match allSlices with
    | fst :: rst -> simplifySlicesHelp fst rst []
    | [] -> failwith "Somehow had an empty list in simplifyAllSlices" in
    
  let flipSliceAlongArc flipArc sliceArc =
    let comparison = cake_compare sliceArc flipArc
    in let _ = if verbose then  print_comp_eval flipArc sliceArc comparison else () 
    in 
    match comparison with
       | NO_OVERLAP -> [sliceArc]
       | OVERLAP_LEFTA ->
           let insideOverlap = { start = flipArc.start;
                                 fin = arcPlus flipArc.start (arcMinus flipArc.fin sliceArc.start);
                                 up = not sliceArc.up }
           in [insideOverlap; { sliceArc with start = flipArc.fin } ]
           
       | OVERLAP_LEFTA_EDGE_RIGHTA ->
           let l = arcMinus sliceArc.fin sliceArc.start
           in [ {start = flipArc.start; fin = arcPlus flipArc.start l ; up = not sliceArc.up } ]
       | A_CONTAINS_B -> (* preserve unchanged parts of A + flipped part *)
           [ { sliceArc with fin = flipArc.start };
             { flipArc with up = not sliceArc.up };
             { sliceArc with start = flipArc.fin } ]
       | B_CONTAINS_A -> (* only flip the a part *)
           let leftDist = arcMinus sliceArc.start flipArc.start
           and rightDist = arcMinus sliceArc.fin flipArc.start
           in [{ start = arcMinus flipArc.fin rightDist; fin = arcMinus flipArc.fin leftDist; up = not sliceArc.up} ]
       | OVERLAP_RIGHTA -> (* only flip the a part!!! *)
           let l = arcMinus sliceArc.fin flipArc.start
           in [ { sliceArc with fin = flipArc.start }; { start = arcMinus flipArc.fin l ; fin = flipArc.fin; up = not sliceArc.up } ]
       | EDGE_LEFTA -> [ { sliceArc with start = flipArc.fin }; {flipArc with up = not sliceArc.up } ]
       | EDGE_LEFTA_OVERLAP_RIGHTA ->
           let l = arcMinus sliceArc.fin sliceArc.start
           in [{ start = arcMinus flipArc.fin l; fin = flipArc.fin; up = not sliceArc.up }] 
       | EQUALS -> [ {sliceArc with up = not sliceArc.up } ]
       | EDGE_RIGHTA -> [ {sliceArc with fin = flipArc.start }; {flipArc with up = not sliceArc.up } ]
       | OVERLAP_BOTH ->
           let l_aLeft = arcMinus flipArc.fin sliceArc.start
           and l_aRight = arcMinus sliceArc.fin flipArc.start
           in [
             (* arc that remains unchanged *)
             { start = flipArc.fin
             ; fin = flipArc.start
             ; up = sliceArc.up };
             (* arc that moves clockwise *)
             { start = arcMinus flipArc.fin l_aRight
             ; fin = flipArc.fin
             ; up = not sliceArc.up };

             { start = flipArc.start
             ; fin = arcPlus flipArc.start l_aLeft
             ; up = not sliceArc.up } ]
  (* apply the flip to all of the slices in the pie *)
  in let apply_flips flip slices =
    let rec update f sl out =
      match sl with
      | s::rst -> update f (rst) (List.concat [flipSliceAlongArc flip s; out])
      | [] -> out
    in update flip slices []

  (* function to check end condition - the whole pie should be up *)
  in let meets_end_condition slices =
    match slices with
    | [] -> failwith "empty pie"
    | (_ :: (_ :: _)) -> NOT_DONE
    | (a :: []) -> if a.up then DONE else REVERSED (* eq a.start a.fin*)
  in let randomArcP () = create_arcPoint (Random.int total_degrees_i) (Random.int total_degrees_i)

  (* random tests *)
  in let rec test_random iters =
    if iters == 0 then ()
    else 
      let flip = {start = randomArcP() ; fin = randomArcP() ; up=false }
      and pie =
        let h1 = randomArcP()
        in let h2 = 
          let rec gen_validh2 () =
            let q = randomArcP()
            in if eq q h1 then gen_validh2() else q
          in gen_validh2()
        in [ { start = h1; fin = h2; up = true }; {start = h2; fin = h1; up = false} ] 
      in let rec sliceExistsInList pie slice =
        match pie with
        | a :: rst -> if (eq slice.start a.start) && (eq slice.fin a.fin) && slice.up == a.up then true
                      else sliceExistsInList rst slice
        | [] -> false
      in let rec test_pie_equality pieA pieB = (* for every arc in pieA, we have to find the identical one in pieB *)
        match pieA with
        | pieAslice :: rstPieA -> if sliceExistsInList pieB pieAslice then test_pie_equality rstPieA pieB else false
        | [] -> true
      in
      let rawFlips = apply_flips flip pie
      in
      let res_arcs = simplifyAllSlices(rawFlips)
      in let res_back = simplifyAllSlices(apply_flips flip res_arcs)
      in 
      if test_pie_equality pie res_back then 
        let _ = if verbose then print_newline() else ()
        in test_random (iters - 1) 
      else
        let _ = Printf.printf "Failed on trial %d: flip( %0.2f(%d %d) - %0.2f(%d %d) ), pie:...\n" iters (flip.start.eval) (fst flip.start.tup) (snd flip.start.tup) (flip.fin.eval) (fst flip.fin.tup) (snd flip.fin.tup)
        and _ = print_arcs(pie)
        and _ = print_newline()
        and _ = print_arcs rawFlips
        and _ = print_newline()
        and _ = print_arcs res_arcs
        and _ = print_newline()
        in print_arcs res_back
  in let rec problem_loop slices f ff fff cursor steps =
    match meets_end_condition slices with
    | DONE -> steps
    | REVERSED -> let _ = print_string "." in steps * 2
    | NOT_DONE ->
    if steps > 100000000 then
      let _ = print_arcs slices
      and _ = Printf.printf "Failing on a(%d) b(%d) c(%d) - %d slices\n" a b c (List.length slices)
       in failwith "something wrong"
    else
      let _ = if verbose then Printf.printf "Step %d has %d slices\n" steps (List.length slices) else ()
      in let cursor_slice =
        let front = cursor
        and back = arcPlus cursor f
        in { start = front; fin = back; up = false }
      in let next_slices = simplifyAllSlices(apply_flips cursor_slice  slices)
      in let _ = if verbose then print_arcs next_slices else ()
      in let _ = if verbose then print_newline() else ()
      in problem_loop next_slices ff fff f (arcPlus cursor f) (steps + 1)
  in
  let rec is_square c i =
    if i > c then -1 
    else if i * i == c then i
    else is_square c (i + 1)
  in match mode with
    | TEST ->
      let _ = if not (is_square c 1 > 0) then test_random test_n else Printf.printf "Skipping because %d\n" c
      in 3
    | DO_PROBLEM -> 
        let sqrt_int = is_square c 1
        and origin = create_arcPoint 0 0
        and aDist = create_arcPoint (b * c) 0
        and bDist = create_arcPoint (a * c) 0 in
        let cDist = if sqrt_int < 0 then create_arcPoint 0 (a * b) else create_arcPoint (sqrt_int * a * b) 0 in
        let starting_slices = [ {start = origin; fin = aDist; up = false}; {start = aDist; fin = origin; up = true}]
        in problem_loop starting_slices bDist cDist aDist aDist 1 
(* do a sanity check *)
let do_many lim =
  let rec loopy a b c sum =
    if a < b && b < c && c <= lim then
      let ans = do_problem a b c DO_PROBLEM in
      let _ = Printf.printf "%d %d %d: %d\n%!" a b c ans
      in loopy a b (c + 1) (sum + ans)
    else if b < lim then loopy a (b + 1) (b + 2) sum
    else if a < lim then loopy (a + 1) (a + 2) (a + 3) sum
    else sum
  in loopy 9 10 11 0;;


let () = Printf.printf "G ="
and loop_max = 17 (*read_int()*)
in Printf.printf "Final sum: %d\n" (do_many loop_max);;

Gc.print_stat stdout;;
(*print_int (do_problem 5 6 8 DO_PROBLEM);;*)









