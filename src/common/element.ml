module type UNIT = sig end

module type S = sig
  type t

  val fresh : hint:string -> t
  val to_string : t -> string

  val equal : t -> t -> bool
  val compare : t -> t -> int

  module Set : Set.S with type elt := t
  module Map : Map.S with type key := t
end

module Make (U : UNIT) : S = struct
  type t =
    { id : int ;
      hint : string }

  let last_id = ref 0
  let fresh ~hint =
    incr last_id;
    { id = !last_id; hint }

  let to_string e = e.hint ^ string_of_int e.id

  let equal e1 e2 = Int.equal e1.id e2.id
  let compare e1 e2 = Int.compare e1.id e2.id

  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)

  module Map = Map.Make(struct
      type nonrec t = t
      let compare = compare
    end)
end
