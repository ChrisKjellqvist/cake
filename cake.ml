type overlap_t =  NO_OVERLAP
  | OVERLAP_LEFTA | OVERLAP_LEFTA_EDGE_RIGHTA
  | A_CONTAINS_B | B_CONTAINS_A
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

type arcPoint = int * int;;

let arcPlus a b = (fst a + fst b, snd a + snd b)
let arcMinus a b = (fst a - fst b, snd a - snd b)

type arc_t = {
  start : arcPoint;
  fin : arcPoint;
  up : bool;
};;

let eq a b = ((fst a) == (fst b)) && ((snd a) == (snd b));;

let a = 3;;
let b = 4;;
let c = 5;;
let sc = sqrt (float_of_int c);;

let total_degrees = float_of_int (360 * a * b * c);;

let rec normalize q =
  if q < 0. then normalize(q +.total_degrees)
  else if q > total_degrees then normalize (q -. total_degrees)
  else q;;

let eval_pos (a, b) = float_of_int a +. float_of_int b *. sc;;

let cake_compare arcA arcB =
  let arcA_p1 = normalize(eval_pos(arcA.start))
  and arcA_p2 = normalize(eval_pos(arcA.fin))
  and arcB_p1 = normalize(eval_pos(arcB.start))
  and arcB_p2 = normalize(eval_pos(arcB.fin))
  in
  if eq arcA.start arcA.fin then A_CONTAINS_B (* the entire circle is covered by A, so necessarily A contains B *)
  else if arcA_p1 < arcA_p2 then (* no boundary-cross 0 *) (
    if arcB_p1 < arcB_p1 then (* no boundary-cross 0 *) (
      if eq arcA.start arcB.start then (
        (* aaa
           bxxx *)
        if arcA_p2 < arcB_p2 then EDGE_LEFTA_OVERLAP_RIGHTA
          (* aaa
             bbbb *)
        else if eq arcA.fin arcB.fin then EQUALS
          (* aaa
             bbb *)
        else if arcA_p2 > arcB_p2 then EDGE_LEFTA
          (* aaa
             bb *)
        else failwith "Uncaught case 0"
      ) else if arcA_p1 < arcB_p1 then (
        (* aaa
            xxxx *)
        if eq arcA.fin arcB.fin then EDGE_RIGHTA
          (* aaa
              bb *)
        else if arcA_p2 < arcB_p1 then NO_OVERLAP
          (* aaa
                b *)
        else if arcB_p2 < arcA_p2 then A_CONTAINS_B
          (* aaa
              b *)
        else if arcB_p2 > arcA_p2 && arcB_p1 < arcA_p2 then OVERLAP_RIGHTA
          (* aaa
               bb *)
        else failwith "Uncaught case 1"
      ) else (
        (* aaa
          bxxxx *)
        if arcB_p2 < arcA_p1 || eq arcB.fin arcA.start then NO_OVERLAP
          (*  aaa
             b *)
        else if eq arcA.fin arcB.fin then OVERLAP_LEFTA_EDGE_RIGHTA
          (*  aaa
             bbbb *)
        else if arcA_p2 < arcB_p2 then B_CONTAINS_A
          (*  aaa
            bbbbbb *)
        else if arcB_p2 > arcA_p1 && arcB_p2 < arcA_p2 then OVERLAP_LEFTA
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
      else if arcA_p1 > arcB_p2 || eq arcA.start arcB.fin then (
          (*bb|bb
           xxx   a..*)
        if arcA_p2 < arcB_p1 || eq arcA.fin arcB.start then NO_OVERLAP
          (* bb|bb
            a     a.. *)
        else if arcA_p2 > arcB_p1 then OVERLAP_RIGHTA
        else failwith "Uncaught case 4"
      ) else if arcA_p1 < arcB_p2 then (
        (* bb|bb
            xx   a...*)
        if arcA_p2 < arcB_p1 || eq arcA.fin arcB.start then OVERLAP_LEFTA
          (* bb|bb
            a    aa*)
        else if arcB_p1 < arcA_p2 then OVERLAP_BOTH
          (* bb|bb
            aaa  aa... *)
        else failwith "Uncaught case 5"
      ) else failwith "Uncaught case 3"
    )
  ) else ( (* arcA overlaps 0 boundary*)
    if arcB_p1 < arcB_p2 then ( (* arcB does not overlap 0 boundary *)
      (* aa|aa
        xx   xx *)
      if arcB_p1 < arcA_p2 then (
          (* aa|aa
            xx  bbx *)
        if eq arcB.fin arcA.fin then EDGE_RIGHTA
          (* aa|aa
                bb *)
        else if arcB_p2 < arcA_p2 then A_CONTAINS_B
          (* aa|aa
                b *)
        else if arcB_p2 < arcA_p2 || eq arcB.fin arcA.start then OVERLAP_RIGHTA
          (* aa|aa
            b   bbb..*)
        else if arcB_p2 > arcA_p1 then OVERLAP_BOTH
        else failwith "Uncaught case 8"
      ) else if arcB_p1 > arcA_p2 || eq arcA.fin arcB.start then (
        (* aa|aa
          xx    b.. *)
        if arcB_p2 < arcA_p1 || eq arcB.fin arcA.start then NO_OVERLAP
          (* aa|aa
            b     b..*)
        else if arcA_p1 < arcB_p2 then OVERLAP_LEFTA
        else failwith "Uncaught case 9"
      ) else if eq arcA.start arcB.start then EDGE_LEFTA
        (* aa|aa
          eb *)
      else if arcB_p1 > arcA_p1 then A_CONTAINS_B
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
        else if arcB_p2 < arcA_p2 then EDGE_LEFTA
          (* aaa|aaa
             bbb|b *)
        else if arcB_p2 > arcA_p2 then EDGE_LEFTA_OVERLAP_RIGHTA
          (* aaa|aaa
             bbb|bbbb *)
        else failwith "Uncaught case 11"
      ) else if arcB_p1 > arcA_p1 then (
        (* aaa|aaa
            bb|bbxxx *)
        if eq arcA.fin arcB.fin then EDGE_RIGHTA
          (* aaa|aaa
               b|bbb *)
        else if arcB_p2 < arcA_p2 then A_CONTAINS_B
          (* aaa|aaa
               b|b    *)
        else if arcB_p2 > arcA_p2 then OVERLAP_RIGHTA
        else failwith "Uncaught case 12"
      ) else if arcB_p1 < arcA_p1 then (
        (* aaa|aaa
          bbbb|bbxxx *)
        if eq arcA.fin arcB.fin then OVERLAP_LEFTA_EDGE_RIGHTA
          (* aaa|aaa
            bbbb|bbb *)
        else if arcB_p2 < arcA_p2 then OVERLAP_LEFTA
          (* aaa|aaa
            bbbb|bb *)
        else if arcB_p2 > arcA_p2 then B_CONTAINS_A
          (* aaa|aaa
            bbbb|bbbb *)
        else failwith "Unncaught case 13"
      ) else failwith "Uncaught case 10"
    )
  );;

let areArcsAdjacent arcA arcB = arcA.up == arcB.up && (eq arcA.start arcB.fin || eq arcA.fin arcB.start) ;;

let mergeAdjacentArcs arcA arcB =
  if eq arcA.start arcB.fin then { arcB with fin = arcA.fin }
  else { arcA with fin = arcB.fin };;

let rec simplifySliceAgainst mSlice slicesToConsider unfitSlices success =
  (* given a slice out, try to combine it with all the others. If it goes, then great: put it in
the "combined list". If it doesn't then put it in the "uncombined list" *)
  match slicesToConsider with
    | headSlice :: rst -> if areArcsAdjacent headSlice mSlice  then simplifySliceAgainst (mergeAdjacentArcs headSlice mSlice) rst unfitSlices true
  else simplifySliceAgainst mSlice rst (headSlice :: unfitSlices) success
    | [] -> (mSlice, unfitSlices, success);;

(* tie all the adecent slices together *)
let simplifyAllSlices allSlices =
  let rec simplifySlicesHelp slices outputSlices =
    match slices with
      | hSlice :: rst ->
          let (resSlice, unmatched, success) = simplifySliceAgainst hSlice rst [] false
          in if success then simplifySlicesHelp (resSlice :: unmatched) outputSlices
    else simplifySlicesHelp rst (resSlice :: outputSlices)
      | [] -> outputSlices
          in simplifySlicesHelp allSlices [] ;;

let flipSliceAlongArc flipArc sliceArc =
  match cake_compare sliceArc flipArc with
     | NO_OVERLAP -> [sliceArc]
     | OVERLAP_LEFTA ->
         let insideOverlap = { start = flipArc.start;
                               fin = arcPlus flipArc.start (arcMinus flipArc.fin sliceArc.start);
                               up = not sliceArc.up }
         in [insideOverlap; { sliceArc with start = flipArc.fin } ]
     | OVERLAP_LEFTA_EDGE_RIGHTA ->
         let outsideArc = { sliceArc with fin = flipArc.start }
         and insideArc = { flipArc with up = not sliceArc.up }
         in [ outsideArc; insideArc ]
     | A_CONTAINS_B -> (* preserve unchanged parts of A + flipped part *)
         [ { sliceArc with fin = flipArc.start };
           { flipArc with up = not flipArc.up };
           { sliceArc with start = flipArc.fin }]
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
         and l_aRight = arcMinus flipArc.start sliceArc.fin
         in [
           { start = flipArc.fin
           ; fin = flipArc.start
           ; up = sliceArc.up };
           { start = arcMinus flipArc.fin l_aRight
           ; fin = flipArc.fin
           ; up = not sliceArc.up };
           { start = flipArc.start
           ; fin = arcPlus flipArc.start l_aLeft
           ; up = not sliceArc.up } ];;

(* let's try some simple test examples to make sure this doesn't explode *)
let sliceA = {
  start = (0, 0);
  fin = (15, 0);
  up = true };;
let flipSlice = {
  start = (5, 0);
  fin = (20, 0);
  up = true };;

print_float total_degrees;; 
print_newline();;
print_string (string_from_overlap ( cake_compare sliceA flipSlice ));;
