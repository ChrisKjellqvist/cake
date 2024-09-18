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

let eq {tup = a; eval = _} {tup = b; eval = _} = ((fst a) == (fst b)) && ((snd a) == (snd b));; 


type run_mode_t =
  | DO_PROBLEM
  | TEST;;

let verbose = false;;
  
let rec print_arcs lst = match lst with
  | a :: rst -> 
      let () = Printf.printf "%d: [ %0.2f(%d, %d), %0.2f(%d, %d) ] %s\n" (List.length rst) (a.start.eval) (fst a.start.tup) (snd a.start.tup) (a.fin.eval) (fst a.fin.tup) (snd a.fin.tup) (if a.up then "up" else "down")
      in print_arcs rst
  | [] -> ();;
  
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
    );;

let print_comp_eval flip slice c =
  Printf.printf "flip (%f, %f) overlaps slice (%f, %f) with %s\n" (flip.start.eval) (flip.fin.eval) (slice.start.eval) (slice.fin.eval) (string_from_overlap c);;


