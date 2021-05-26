open Common
open AST

type parsetree =
  | EProd
  | TProd of terminal
  | NTProd of (CNF.nonterminal * parsetree) * (CNF.nonterminal * parsetree)

let parse g s =
  (* input: grammar g containing [r] nonterminals [0] to [r-1] with start symbol [0]. *)
  (* input: an array s consisting of [n] "characters" [s0] to [sn-1]. *)
  let r = CNF.number_of_nonterminals g in
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
    CNF.iter_terminal_productions
      (fun nt t ->
         if t = s.(i) then
           p.(0).(i).(CNF.nonterminal_index nt) <- Some (TProd t))
      g
  done;

  for l = 1 to n-1 do (* length of span *)
    for i = 0 to n-l-1 do (* start of span *)
      for k = 0 to l-1 do (* partition of span *)
        CNF.iter_nonterminal_productions
          (fun nt b c ->
             match
               p.(k).(i).(CNF.nonterminal_index b),
               p.(l-k-1).(i+k+1).(CNF.nonterminal_index c)
             with
             | Some ptb, Some ptc ->
               let nt = CNF.nonterminal_index nt in
               p.(l).(i).(nt) <- Some (NTProd ((b, ptb), (c, ptc)))
             | _ -> ())
          g
       done
    done
  done;

  p.(n-1).(0).(0)

let parse g s =
  let s = Array.of_seq (Word.letters s) in
  if s = [||] then
    if CNF.recognises_empty g then Some EProd
    else None
  else parse g s

let accepts g s =
  parse g s <> None

let%test_module _ =
  (module struct
    let t s = T (Letter.from_string s)

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
          (vp,  [t "eats"]);    (*  VP -> eats   *)
          (pp,  [N p;   N np]); (*  PP ->  P  NP *)
          (np,  [N det; N n ]); (*  NP -> Det N  *)
          (np,  [t "she"]);     (*  NP -> she    *)
          (v,   [t "eats"]);    (*   V -> eats   *)
          (p,   [t "with"]);    (*   P -> with   *)
          (n,   [t "fish"]);    (*   N -> fish   *)
          (n,   [t "fork"]);    (*   N -> fork   *)
          (det, [t "a"]);       (* Det -> a      *)
        ]
      in
      CNF.from_cfg cfg

    let accepts cnf w =
      accepts cnf (Word.from_letters_list (List.map Letter.from_string w))

    let%test _ = accepts cnf ["she"; "eats"; "a"; "fish"; "with"; "a"; "fork"]
    let%test _ = not (accepts cnf ["she"; "eats"; "fish"; "with"; "fork"])
  end)
