open Cake;;

let do_problem a b c mode =
  let test_n = a * b * c * 10 in
  let sc = sqrt (float_of_int c)
  and total_degrees = float_of_int (a * b * c)
  and total_degrees_i = a * b * c in

  let pieA = Dynarray.create()
  and pieB = Dynarray.create() in 
  (* Prior, merge was a list -> list function, creating a bit of allocator overhead.
     If we do it in place, then we have to do something special so that we can remove
     elements while merging. 

     Prior:
       | A | B | C | D .. | Y | Z |
           V       V          V
       | AB    |  CD  |.. |  YZ   |

     This map is fine because we produce the stream AB, CD, ... and just add them to
     the list. Instead, we just maintain two Dynarrays for consecutive iterations. *)

  let create_arcPoint a b = (
    let eval_pos a b = 
      let normalize q =
        if q < 0. then q +. Float.abs(Float.floor(q /. total_degrees)) *. total_degrees
        else if q > total_degrees then q -. Float.floor(q /. total_degrees) *. total_degrees 
        else q in
      let r = normalize(float_of_int a +. float_of_int b *. sc) in
      if r < 0. then failwith (String.concat " " [string_of_float r; string_of_float (float_of_int a +. float_of_int b *. sc)])
      else r in
    let modNorm q =
      if q >= total_degrees_i then q mod total_degrees_i
      else if q < 0 then total_degrees_i + (q mod total_degrees_i)
      else q in
    let fixA = modNorm a in
    { tup = (fixA, b); eval = eval_pos fixA b } ) in

  let arcPlus a b = create_arcPoint (fst a.tup + fst b.tup) (snd a.tup + snd b.tup) in 
  let arcMinus a b = create_arcPoint (fst a.tup - fst b.tup) (snd a.tup - snd b.tup)
  and verbose = false in 

  let areArcsAdjacent arcA arcB = arcA.up == arcB.up && (eq arcA.start arcB.fin || eq arcA.fin arcB.start) in

  let simplifyAllSlices pieSource pieSink = 
    let () = Quicksort.generic_qsort (Dynarray.get pieSource) (Dynarray.set pieSource) (fun (a: arc_t) (b: arc_t) -> Float.compare a.start.eval b.start.eval) 0 (Dynarray.length pieSource); in
    let rec simplifySlices pieSrc pieSink headSlice inspectIdx = (
      if inspectIdx == Dynarray.length pieSrc then
        if Dynarray.is_empty pieSink then
          Dynarray.add_last pieSink headSlice (* just one slice in the array *)
        else (* many slices. try to merge headSlice into the fist in the sink array *)
          let headSinkSlice = Dynarray.get pieSink 0 in
          if areArcsAdjacent headSlice headSinkSlice  then (* headSlice is adjacent to the beginning, directly merge *)
            Dynarray.set pieSink 0 { headSinkSlice with start = headSlice.start }
          else 
            Dynarray.add_last pieSink headSlice
      else 
        let inspect = Dynarray.get pieSrc inspectIdx in 
        if areArcsAdjacent inspect headSlice then
          let mergedArc = { headSlice with fin = inspect.fin } in
          simplifySlices pieSrc pieSink mergedArc (inspectIdx + 1)
      else
        let _ = Dynarray.add_last pieSink headSlice in 
        simplifySlices pieSrc pieSink inspect (inspectIdx + 1))
      in simplifySlices pieSource pieSink (Dynarray.get pieSource 0) 1  in

    let flipSliceAlongArc flipArc sliceArc i pie =
      let comparison = cake_compare sliceArc flipArc in
      let _ = if verbose then  print_comp_eval flipArc sliceArc comparison else () in 
      match comparison with
      | NO_OVERLAP -> () (* [sliceArc] - do nothing *)
      | OVERLAP_LEFTA ->
          let insideOverlap = { start = flipArc.start;
                                     fin = arcPlus flipArc.start (arcMinus flipArc.fin sliceArc.start);
                                     up = not sliceArc.up } in 
          Dynarray.add_last pie insideOverlap ; 
                Dynarray.set pie i { sliceArc with start = flipArc.fin }
      | OVERLAP_LEFTA_EDGE_RIGHTA ->
          let l = arcMinus sliceArc.fin sliceArc.start
               in Dynarray.set pie i {start = flipArc.start; fin = arcPlus flipArc.start l ; up = not sliceArc.up }
      | A_CONTAINS_B -> (* preserve unchanged parts of A + flipped part *)
          Dynarray.set pie i { sliceArc with fin = flipArc.start };
               Dynarray.add_last pie { flipArc with up = not sliceArc.up };
               Dynarray.add_last pie { sliceArc with start = flipArc.fin }
      | B_CONTAINS_A -> (* only flip the a part *)
          let leftDist = arcMinus sliceArc.start flipArc.start
               and rightDist = arcMinus sliceArc.fin flipArc.start in
          Dynarray.set pie i { start = arcMinus flipArc.fin rightDist; fin = arcMinus flipArc.fin leftDist; up = not sliceArc.up}
      | OVERLAP_RIGHTA -> (* only flip the a part!!! *)
          let l = arcMinus sliceArc.fin flipArc.start in 
          Dynarray.set pie i { sliceArc with fin = flipArc.start };
               Dynarray.add_last pie { start = arcMinus flipArc.fin l ; fin = flipArc.fin; up = not sliceArc.up }
      | EDGE_LEFTA ->
          Dynarray.set pie i { sliceArc with start = flipArc.fin };
               Dynarray.add_last pie {flipArc with up = not sliceArc.up }
      | EDGE_LEFTA_OVERLAP_RIGHTA ->
          let l = arcMinus sliceArc.fin sliceArc.start
               in Dynarray.set pie i { start = arcMinus flipArc.fin l; fin = flipArc.fin; up = not sliceArc.up } 
      | EQUALS -> Dynarray.set pie i {sliceArc with up = not sliceArc.up }
      | EDGE_RIGHTA -> 
          Dynarray.set pie i {sliceArc with fin = flipArc.start }; 
               Dynarray.add_last pie { flipArc with up = not sliceArc.up }
      | OVERLAP_BOTH ->
          let l_aLeft = arcMinus flipArc.fin sliceArc.start
               and l_aRight = arcMinus sliceArc.fin flipArc.start
               in Dynarray.set pie i
                 (* arc that remains unchanged *)
                 { start = flipArc.fin
                 ; fin = flipArc.start
                 ; up = sliceArc.up };
                 (* arc that moves clockwise *)
                 Dynarray.add_last pie 
                 { start = arcMinus flipArc.fin l_aRight
                 ; fin = flipArc.fin
                 ; up = not sliceArc.up };
                 Dynarray.add_last pie 
                 { start = flipArc.start
                 ; fin = arcPlus flipArc.start l_aLeft
                 ; up = not sliceArc.up } in 

    let apply_flips flip slices = (
      let n = Dynarray.length slices in 
      let rec update i =
        if i == n then ()
        else
          let _ = flipSliceAlongArc flip (Dynarray.get slices i) i slices in
          update (i + 1)
          in update 0 ) in 

    (* function to check end condition - the whole pie should be up *)
    let meets_end_condition slices = (
      if Dynarray.length slices = 1 then (if (Dynarray.get slices 0).up then DONE else REVERSED)
      else NOT_DONE ) in

    let rec test_random iters = (
      let randomArcP () = create_arcPoint (Random.int total_degrees_i) (Random.int total_degrees_i) in 
      if iters == 0 then ()
      else 
        let flip = {start = randomArcP() ; fin = randomArcP() ; up=false }
        and pieO = ( 
          let pieO = Dynarray.create() in
          let _ = Dynarray.clear pieO in 
          let h1 = randomArcP()
          in let h2 = 
            let rec gen_validh2 () =
              let q = randomArcP()
              in if eq q h1 then gen_validh2() else q
              in gen_validh2()
            in Dynarray.add_last pieO { start = h1; fin = h2; up = true };
             Dynarray.add_last pieO {start = h2; fin = h1; up = false}; 
             pieO ) in  

        let rec test_pie_equality pieA pieB idx =
          if idx == Dynarray.length pieA then true
          else 
            let cEle = Dynarray.get pieA idx in 
            if not (Dynarray.exists (fun x -> (eq x.start cEle.start) && (eq x.fin cEle.fin)) pieB) then false
            else test_pie_equality pieA pieB (idx + 1) in 

        (* flip pieO into pieA into pieB, pieO and pieB should be identical afterwards *)
        let original_save = Dynarray.copy pieO in
        let () = 
                 Dynarray.clear pieA; Dynarray.clear pieB;
                 apply_flips flip pieO;
                 simplifyAllSlices pieO pieA;
                 apply_flips flip pieA;
                 simplifyAllSlices pieA pieB;
        in
        if test_pie_equality original_save pieB 0 then 
          test_random (iters - 1) 
        else
          let _ = Printf.printf "Failed on trial %d: flip( %0.2f(%d %d) - %0.2f(%d %d) ), pie:...\n" iters (flip.start.eval) (fst flip.start.tup) (snd flip.start.tup) (flip.fin.eval) (fst flip.fin.tup) (snd flip.fin.tup) in 
            print_arcs pieB) in 

    let rec problem_loop f ff fff cursor steps src sink =
      match meets_end_condition src with
      | DONE -> steps
      | REVERSED -> let _ = print_string "." in steps * 2
      | NOT_DONE ->
          if steps > 100000000 then
            let _ = print_arcs src
        and _ = Printf.printf "Failing on a(%d) b(%d) c(%d)\n" a b c
         in failwith "something wrong"
        else
          let cursor_slice =
            let front = cursor
            and back = arcPlus cursor f in 
            { start = front; fin = back; up = false } in
          let _ = apply_flips cursor_slice src in
          let _ = Dynarray.clear sink in
          let _ = simplifyAllSlices src sink in 
          problem_loop ff fff f (arcPlus cursor f) (steps + 1) sink src in

    let rec is_square c i =
      if i > c then -1 
      else if i * i == c then i
      else is_square c (i + 1) in

    match mode with
      | TEST ->
          let _ = 
            if not (is_square c 1 > 0) then test_random test_n 
          else Printf.printf "Skipping because %d\n" c in
          3
      | DO_PROBLEM -> 
          let sqrt_int = is_square c 1
        and origin = create_arcPoint 0 0
        and aDist = create_arcPoint (b * c) 0
        and bDist = create_arcPoint (a * c) 0 in
          let cDist = 
            if sqrt_int < 0 then create_arcPoint 0 (a * b) 
            else create_arcPoint (sqrt_int * a * b) 0 in
          let _ = Dynarray.append_list pieA [ {start = origin; fin = aDist; up = false}; {start = aDist; fin = origin; up = true}] in
          problem_loop bDist cDist aDist aDist 1 pieA pieB;; 

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
  and loop_max = 53 (*read_int()*)
  in Printf.printf "Final sum: %d\n" (do_many loop_max);;

Gc.print_stat stdout;;
(*print_int (do_problem 5 6 8 DO_PROBLEM);;*)
