type 'a t = 'a list [@@deriving show { with_path = false }]

let empty = []

let is_empty s = s = []

let push x s = x :: s

let pop_opt = function
  | [] -> None
  | x :: s -> Some (x, s)

let pop s =
  match pop_opt s with
  | None -> failwith "Stack.pop"
  | Some (x, s) -> (x, s)

let equal = ExtList.equal
let compare = ExtList.compare
