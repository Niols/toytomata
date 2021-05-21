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
  { empty = !empty; t_prod; nt_prod; hints }

type parsetree =
  | EProd
  | TProd of terminal
  | NTProd of (int * parsetree) * (int * parsetree)

let parse g s =
  (* input: grammar g containing [r] nonterminals [0] to [r-1] with start symbol [0]. *)
  (* input: an array s consisting of [n] "characters" [s0] to [sn-1]. *)
  let r = Array.length g.nt_prod in
  let n = Array.length s in

  (* create an array of booleans of sizes n, n and r initialised to [false].
     actually, we do not need all of the second [n]. *)
  (* p[l,i,nt] will be set to [true] if the substring of length [l+1] starting
     from [i] can be generated from the nonterminal [nt]. *)
  let p =
    Array.init n @@ fun l ->
    Array.init (n-l) @@ fun _ ->
    Array.make r None
  in

  for i = 0 to n-1 do
    Array.iteri
      (fun nt ts ->
         List.iter
           (fun t ->
              if t = s.(i) then
                p.(0).(i).(nt) <- Some (TProd t))
           ts)
      g.t_prod
  done;

  for l = 1 to n-1 do (* length of span *)
    for i = 0 to n-l-1 do (* start of span *)
      for k = 0 to l-1 do (* partition of span *)
        Array.iteri
          (fun nt ntps ->
             List.iter
               (fun (b, c) ->
                  match p.(k).(i).(b), p.(l-k-1).(i+k+1).(c) with
                  | Some ptb, Some ptc ->
                    p.(l).(i).(nt) <- Some (NTProd ((b, ptb), (c, ptc)))
                  | _ -> ())
               ntps)
          g.nt_prod
      done
    done
  done;

  p.(n-1).(0).(0)

let parse g s =
  if s = [||] then
    if g.empty then Some EProd
    else None
  else parse g s

let pp_parsetree g fmt (i, pt) =
  let rec pp_parsetree fmt (i, pt) =
    match pt with
    | EProd ->
      Format.fprintf fmt "%s -> ε"
        (NonTerminal.to_string g.hints.(i))
    | TProd t ->
      Format.fprintf fmt "%s -> %s"
        (NonTerminal.to_string g.hints.(i))
        t
    | NTProd ((b, ptb), (c, ptc)) ->
      Format.fprintf fmt "@[<h 2>%s -> %s %s@\n%a@\n%a@]"
        (NonTerminal.to_string g.hints.(i))
        (NonTerminal.to_string g.hints.(b))
        (NonTerminal.to_string g.hints.(c))
        pp_parsetree (b, ptb)
        pp_parsetree (c, ptc)
  in
  pp_parsetree fmt (i, pt)

let accepts g s =
  parse g s <> None

let%test_module _ =
  (module struct
    let cnf =
      let cfg =
        (* Example taken from Wikipedia *)
        let s =   fresh_nonterminal ~hint:"S"   () in
        let np =  fresh_nonterminal ~hint:"NP"  () in
        let vp =  fresh_nonterminal ~hint:"VP"  () in
        let pp =  fresh_nonterminal ~hint:"PP"  () in
        let v =   fresh_nonterminal ~hint:"V"   () in
        let p =   fresh_nonterminal ~hint:"P"   () in
        let det = fresh_nonterminal ~hint:"Det" () in
        let n =   fresh_nonterminal ~hint:"N"   () in
        empty_cfg
        |> add_entrypoint s
        |> add_productionss [
          (s,   [N np;  N vp]); (*   S ->  NP VP *)
          (vp,  [N vp;  N pp]); (*  VP ->  VP PP *)
          (vp,  [N v;   N np]); (*  VP ->  V  NP *)
          (vp,  [T "eats"]);    (*  VP -> eats   *)
          (pp,  [N p;   N np]); (*  PP ->  P  NP *)
          (np,  [N det; N n ]); (*  NP -> Det N  *)
          (np,  [T "she"]);     (*  NP -> she    *)
          (v,   [T "eats"]);    (*   V -> eats   *)
          (p,   [T "with"]);    (*   P -> with   *)
          (n,   [T "fish"]);    (*   N -> fish   *)
          (n,   [T "fork"]);    (*   N -> fork   *)
          (det, [T "a"]);       (* Det -> a      *)
        ]
      in
      from_cfg cfg

    let%test _ = accepts cnf [|"she"; "eats"; "a"; "fish"; "with"; "a"; "fork"|]
    let%test _ = not (accepts cnf [|"she"; "eats"; "fish"; "with"; "fork"|])
  end)
