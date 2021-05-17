type ('a, 'b) converter =
  { table : ('a, 'b) Hashtbl.t ;
    maker : 'a -> 'b }

let make_converter maker =
  let table = Hashtbl.create 8 in
  { table; maker }

let convert converter x =
  match Hashtbl.find_opt converter.table x with
  | None ->
    let y = converter.maker x in
    Hashtbl.add converter.table x y;
    y
  | Some y ->
    y

let make_convert maker =
  convert (make_converter maker)

let make_int_convert () =
  let counter = ref 0 in
  make_convert (fun _x ->
      let y = !counter in
      incr counter;
      y
    )
