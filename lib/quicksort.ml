
let generic_qsort (getF: int -> 'a) (setF: int -> 'a -> unit) (compare: 'a -> 'a -> int) (beginIdx: int) (stopIdx: int) =

  let [@inline] swap idxA idxB =
    if idxA = idxB then ()
    else 
      let a = getF idxA
      and b = getF idxB
      in setF idxB a ; setF idxA b in

  let rec partition_loop pivot loIdx hiIdx pivotIdx =
    if loIdx == hiIdx then pivotIdx
    else
      let cEle = getF loIdx in
      let comparison = compare cEle pivot in
      if comparison < 0 then 
        let () = swap pivotIdx loIdx
        in partition_loop pivot (loIdx + 1) hiIdx (pivotIdx + 1)
      else partition_loop pivot (loIdx + 1) hiIdx pivotIdx in

  let partition loIdx hiIdx =
    let pivotIdx = (loIdx + hiIdx) lsr 1 in
    let pivotA = getF pivotIdx in
    let _ = swap (hiIdx - 1) pivotIdx in 
    let pivotLoc = partition_loop pivotA loIdx (hiIdx - 1) loIdx in
    let _ = swap pivotLoc (hiIdx - 1) in
    pivotLoc in

  let rec qsort start stop =
    if stop - start <= 1 then ()
    else
      let partition_point = partition start stop
      in qsort start partition_point ; qsort (partition_point + 1) stop in
  qsort beginIdx stopIdx;;

