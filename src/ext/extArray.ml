include Array

let fold_lefti f x a =
  let n = Array.length a in
  let rec fold_lefti i x =
    if i < n then
      fold_lefti (i+1) (f x i a.(i))
    else
      x
  in
  fold_lefti 0 x

let equal eq a b =
  List.equal eq (Array.to_list a) (Array.to_list b)

let compare cmp a b =
  List.compare cmp (Array.to_list a) (Array.to_list b)
