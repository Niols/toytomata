include Seq

let rec flatten ss =
  fun () ->
  match ss () with
  | Nil -> Nil
  | Cons (s, ss) -> append s (flatten ss) ()
