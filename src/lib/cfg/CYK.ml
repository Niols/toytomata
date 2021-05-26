open Ext
open Common
open AST

type parsetree =
  | EProd
  | TProd of terminal
  | NTProd of (CNF.nonterminal * parsetree) * (CNF.nonterminal * parsetree)

let parse cnf a =
  (* input: grammar [cnf] containing [r] nonterminals. *)
  (* input: an array [a] consisting of [n] "characters" [a1] to [an]. *)
  let r = CNF.number_of_nonterminals cnf in
  let n = Array.length a in

  (* create an array of booleans of sizes n, n and r initialised to [false].
     actually, we do not need all of the second [n]. *)
  (* p[l,i,nt] will be set to [true] if the substring of length [l] starting
     from [i] can be generated from the nonterminal [nt]. *)
  let p =
    Array.init n @@ fun l ->
    Array.init (n-l) @@ fun _ ->
    Array.make r None
  in

  (* the -1 are here to respect the convention of the algorithm as presented on
     https://en.wikipedia.org/wiki/CYK_algorithm *)
  let a s = a.(s-1) in
  let get l s nt = p.(l-1).(s-1).(CNF.nonterminal_index nt) in
  let set l s nt value = p.(l-1).(s-1).(CNF.nonterminal_index nt) <- Some value in

  for s = 1 to n do
    CNF.iter_terminal_productions
      (fun nt t ->
         if Letter.equal t (a s) then
           set 1 s nt (TProd t))
      cnf
  done;

  for l = 2 to n do (* length of span *)
    for s = 1 to n-l+1 do (* start of span *)
      for p = 1 to l-1 do (* partition of span *)
        CNF.iter_nonterminal_productions
          (fun ra rb rc ->
             match
               get p s rb,
               get (l-p) (s+p) rc
             with
             | Some ptb, Some ptc ->
               set l s ra (NTProd ((rb, ptb), (rc, ptc)))
             | _ -> ())
          cnf
       done
    done
  done;

  get n 1 (CNF.start cnf)

let parse g s =
  let s = Array.of_seq (Word.letters s) in
  if s = [||] then
    if CNF.recognises_empty g then Some EProd
    else None
  else parse g s

let accepts g s =
  parse g s <> None

module State = struct
  module IMap = Map.Make(Int)

  type t =
    { cnf : CNF.t ;
      word : Word.t ;
      trees : parsetree IMap.t IMap.t IMap.t }

  let initial cnf =
    { cnf ;
      word = Word.empty ;
      trees = IMap.empty }

  let get i j nt trees =
    match IMap.find_opt i trees with
    | None -> None
    | Some trees ->
      match IMap.find_opt j trees with
      | None -> None
      | Some trees ->
        let nt = CNF.nonterminal_index nt in
        IMap.find_opt nt trees

  let set i j nt trees value =
    assert (i > 0);
    assert (j > 0);
    IMap.update i
      (fun trees ->
         let trees = match trees with
           | None -> IMap.empty
           | Some trees -> trees
         in
         let trees =
           IMap.update j
             (fun trees ->
                let trees = match trees with
                  | None -> IMap.empty
                  | Some trees -> trees
                in
                let trees =
                  IMap.update (CNF.nonterminal_index nt)
                    (fun _ -> Some value)
                    trees
                in
                Some trees)
             trees
         in
         Some trees)
      trees

  let parse_letter state letter =
    let cnf = state.cnf in
    let word = Word.add_letter state.word letter in
    let n = Word.length word in (* index of [letter] in [word] *)
    let trees = state.trees in

    let trees =
      CNF.fold_terminal_productions
        (fun trees nt t ->
           if Letter.equal letter t then
             set 1 n nt trees (TProd t)
           else trees
        )
        trees
        cnf
    in

    let trees =
      fold_for_loop ~from_:2 ~to_:n trees (fun trees l -> (* length of span *)
          let s = n - l + 1 in (* start of span *)
          if s > 0 then
            (
              fold_for_loop ~from_:1 ~to_:(l-1) trees (fun trees p -> (* partition of span *)
                  CNF.fold_nonterminal_productions
                    (fun trees ra rb rc ->
                       match
                         get p s rb trees,
                         get (l-p) (s+p) rc trees
                       with
                       | Some ptb, Some ptc ->
                         set l s ra trees (NTProd ((rb, ptb), (rc, ptc)))
                       | _ -> trees)
                    trees
                    cnf)
            )
          else
            trees
        )
    in

    { cnf ; word ; trees }

  let parse_word state word =
    Word.letters word
    |> Seq.fold_left parse_letter state

  let parsetree state =
    if Word.is_empty state.word then
      if CNF.recognises_empty state.cnf then
        Some EProd
      else
        None
    else
      get (Word.length state.word) 1 (CNF.start state.cnf) state.trees

  let accepting state =
    parsetree state <> None
end

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

    (* Direct parsing *)

    let accepts word =
      let word = Word.from_letters_list (List.map Letter.from_string word) in
      accepts cnf word

    let%test _ = not (accepts [])
    let%test _ = not (accepts ["she"])
    let%test _ = accepts ["she"; "eats"]
    let%test _ = accepts ["she"; "eats"; "a"; "fish"; "with"; "a"; "fork"]
    let%test _ = not (accepts ["she"; "eats"; "fish"; "with"; "fork"])

    (* Incremental parsing *)

    let accepts word =
      let word = Word.from_letters_list (List.map Letter.from_string word) in
      State.(accepting (parse_word (initial cnf) word))

    let%test _ = not (accepts [])
    let%test _ = not (accepts ["she"])
    let%test _ = accepts ["she"; "eats"]
    let%test _ = accepts ["she"; "eats"; "a"; "fish"; "with"; "a"; "fork"]
    let%test _ = not (accepts ["she"; "eats"; "fish"; "with"; "fork"])
  end)
