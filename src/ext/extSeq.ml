include Seq

let rec append s1 s2 =
  fun () ->
  match s1 () with
  | Nil -> s2 ()
  | Cons (x1, s1) ->
    Cons (x1, append s1 s2)

let rec flatten ss =
  fun () ->
  match ss () with
  | Nil -> Nil
  | Cons (s, ss) -> append s (flatten ss) ()

let iteri f s =
  let rec iteri i s =
    match s () with
    | Nil -> ()
    | Cons (x, s) ->
      f i x;
      iteri (i+1) s
  in
  iteri 0 s

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

let fold_left_pairs f a s s' =
  fold_left (fun a e -> fold_left (fun a e' -> f a e e') a s') a s
