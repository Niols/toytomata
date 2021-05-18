include Seq

let rec flatten ss =
  fun () ->
  match ss () with
  | Nil -> Nil
  | Cons (s, ss) -> append s (flatten ss) ()

let rec iter2 f s1 s2 =
  match s1 (), s2 () with
  | Nil, Nil -> ()
  | Nil, _ | _, Nil -> invalid_arg "ExtSeq.iter2"
  | Cons (x1, s1), Cons (x2, s2) ->
    f x1 x2;
    iter2 f s1 s2

let rec map2 f s1 s2 =
  fun () ->
  match s1 (), s2 () with
  | Nil, Nil -> Nil
  | Nil, _ | _, Nil -> invalid_arg "ExtSeq.map2"
  | Cons (x1, s1), Cons (x2, s2) ->
    Cons (f x1 x2, map2 f s1 s2)
