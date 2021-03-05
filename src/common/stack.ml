type 'a t = 'a list

let empty = []

let push x s = x :: s

let pop_opt = function
  | [] -> None
  | x :: _ -> Some x

let pop s =
  match pop_opt s with
  | None -> failwith "Stack.pop"
  | Some x -> x
