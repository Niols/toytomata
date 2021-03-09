module type UNIT = sig end

module type S = sig
  type t

  val fresh : hint:string -> t
  val to_string : t -> string

  val equal : t -> t -> bool
  val compare : t -> t -> int
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
end
