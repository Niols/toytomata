open Ext
open Common
open AST

type t =
  { empty : bool ;                     (* whether [ε] is produced by [0] or not *)
    t_prod : terminal list array ;     (* prod. of the form [A -> a] *)
    nt_prod : (int * int) list array ; (* prod. of the form [A -> BC] *)
    hints : nonterminal array }        (* [0] is always [S] *)

type nonterminal = int

let nonterminal_index = Fun.id
let nonterminal_from_index _ = Fun.id

let nonterminal_to_ast cnf nt = cnf.hints.(nt)

let recognises_empty cnf = cnf.empty

let start _ = 0

let iter_terminal_productions f cnf =
  Array.iteri (fun a -> List.iter (fun v -> f a v)) cnf.t_prod

let fold_terminal_productions f x cnf =
  Array.fold_lefti (fun x a -> List.fold_left (fun x v -> f x a v) x) x cnf.t_prod

let iter_nonterminal_productions f cnf =
  Array.iteri (fun a -> List.iter (fun (b, c) -> f a b c)) cnf.nt_prod

let fold_nonterminal_productions f x cnf =
  Array.fold_lefti (fun x a -> List.fold_left (fun x (b, c) -> f x a b c) x) x cnf.nt_prod

let check cnf =
  let n = Array.length cnf.t_prod in
  assert (Array.length cnf.nt_prod = n);
  assert (Array.length cnf.hints = n);
  iter_nonterminal_productions
    (fun _a b c ->
       assert (b < n);
       assert (c < n);
       assert (b <> 0);
       assert (c <> 0))
    cnf

let from_cfg cfg =
  (* Ready an int_converter, but keep the counter visible *)
  let nb_nonterminals = ref (-1) in
  let convert = Converter.make_convert (fun _ -> incr nb_nonterminals; !nb_nonterminals) in

  (* Convert all non-terminals, starting with the (unique) entrypoint *)
  (match entrypoints cfg with
   | [s] -> assert (convert s = 0)
   | _ -> failwith "CNF.from_cfg: can only have exactly one entrypoint");
  nonterminals cfg |> Seq.iter (fun nt -> ignore (convert nt));

  (* Create the fields of the CNF *)
  let empty = ref false in
  let t_prod = Array.make (1 + !nb_nonterminals) [] in
  let nt_prod = Array.make (1 + !nb_nonterminals) [] in
  let hints = Array.make (1 + !nb_nonterminals) (Obj.magic 0) in (* FIXME: dirty AF *)

  (* Register all the hints *)
  nonterminals cfg |> Seq.iter (fun nt ->
      hints.(convert nt) <- nt
    );

  (* Crawl through the productions to register them *)
  productions cfg |> Seq.iter (fun (nt, p) ->
      let nt = convert nt in
      match p with
      | [] when nt = 0 -> empty := true
      | [T a] -> t_prod.(nt) <- a :: t_prod.(nt)
      | [N b; N c] ->
        let b = convert b and c = convert c in
        if b = 0 || c = 0 then
          failwith "CNF.from_cfg: no rule is allowed to produce the start symbol";
        nt_prod.(nt) <- (b, c) :: nt_prod.(nt)
      | _ -> failwith "CNF.from_cfg: only rules of the form A -> BC, A -> a and S -> ε are allowed"
    );

  (* Return *)
  let cnf = { empty = !empty; t_prod; nt_prod; hints } in
  check cnf;
  cnf

let number_of_nonterminals cnf =
  Array.length cnf.nt_prod
