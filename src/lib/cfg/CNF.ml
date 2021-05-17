(** {1 Chomsky Normal Form}

   Efficient representation of grammars in Chomsky normal forms.

   A context-free grammar is said to be in Chomsky normal form if all of its
   production rules are of the form: [A -> BC] or [A -> a] or [S -> ε] where
   [A], [B] and [C] are nonterminal symbols, the letter [a] is a terminal
   symbol, [S] is the start symbol and [ε] denotes the empty string. Also,
   neither [B] nor [C] may be the start symbol, and the third production rule
   can only appear if [ε] is in the language produced by the grammar. *)

open Common
open AST

type cnf =
  { empty : bool ;                     (* whether [ε] is produced by [0] or not *)
    t_prod : terminal list array ;     (* prod. of the form [A -> a] *)
    nt_prod : (int * int) list array ; (* prod. of the form [A -> BC] *)
    hints : nonterminal array }        (* [0] is always [S] *)

let from_cfg cfg =
  (* Ready an int_converter, but keep the counter visible *)
  let nb_nonterminals = ref (-1) in
  let convert = Converter.make_convert (fun _ -> incr nb_nonterminals; !nb_nonterminals) in

  (* Convert all non-terminals, starting with the (unique) entrypoint *)
  (match entrypoints cfg with
   | [s] -> assert (convert s = 0)
   | _ -> failwith "CNF.from_cfg: can only have exactly one entrypoint");
  nonterminals cfg |> List.iter (fun nt -> ignore (convert nt));

  (* Create the fields of the CNF *)
  let empty = ref false in
  let t_prod = Array.make (1 + !nb_nonterminals) [] in
  let nt_prod = Array.make (1 + !nb_nonterminals) [] in
  let hints = Array.make (1 + !nb_nonterminals) (Obj.magic 0) in (* FIXME: dirty AF *)

  (* Register all the hints *)
  nonterminals cfg |> List.iter (fun nt ->
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
  { empty = !empty; t_prod; nt_prod; hints }
