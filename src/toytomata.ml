open Common

type kind = PDA | CFG
type source = FromStdin | FromFile of string | FromString of string
type input =
  { mutable kind : kind ;
    mutable source : source ;
    mutable source_set : bool }

let default_input kind =
  { kind;
    source = FromStdin; source_set = false }

let inputs = ref []
let add_input kind = inputs := (default_input kind) :: !inputs
let cur_input () = List.hd !inputs
let all_inputs () = List.rev !inputs

let default_nb_words = 10
let nb_words = ref default_nb_words

let usage_str =
  spf "%s KIND [OPTIONS] [KIND [OPTIONS] ...]

KIND can be PDA or CFG.
OPTIONS can be:" Sys.argv.(0)

let anonymous str =
  add_input (
    match String.lowercase_ascii str with
    | "pda" -> PDA
    | "cfg" -> CFG
    | _ -> raise (Arg.Bad (spf "unkown object kind `%s`" str))
  )

let set_source source =
  let input = cur_input () in
  if input.source_set then
    raise (Arg.Bad (spf "source already set"));
  input.source <- source;
  input.source_set <- true

let set_source_from_file fname = set_source (FromFile fname)
let set_source_from_string str = set_source (FromString str)
let set_source_from_stdin () = set_source FromStdin

let spec =
  Arg.[
    "--from-file",   String set_source_from_file,  "FILE Gets input from given file";
    "-f",            String set_source_from_file,  "FILE Short for --from-file";
    "--from-string", String set_source_from_string, "STR Gets input from given string";
    "-s",            String set_source_from_string, "STR Short for --from-string";
    "--from-stdin",  Unit   set_source_from_stdin,     " Gets input from stdin (default)";
    "--nb-words",    Int    ((:=) nb_words),    (spf "NB Print NB words (default: %d)" default_nb_words);

  ] |> Arg.align

let () =
  Arg.parse spec anonymous usage_str

let pdas =
  all_inputs () |> List.mapi @@ fun i input ->
  pf "@\nPDA #%d:@." (i + 1);
  match input.kind with
  | PDA ->
    (
      match input.source with
      | FromFile fname ->
        pf "Reading from file \"%s\".@." fname;
        PDA.from_file fname
      | FromString str ->
        pf "Reading from string \"%s\".@." str;
        PDA.from_string str
      | FromStdin ->
        pf "Please enter a PDA (end it with Ctrl+D):@.";
        PDA.from_channel stdin
    )
  | CFG ->
    (
      pf "Getting a CFG and converting it later.@.";
      let cfg =
        match input.source with
        | FromFile fname ->
          pf "Reading from file \"%s\".@." fname;
          CFG.from_file fname
        | FromString str ->
          pf "Reading from string \"%s\".@." str;
          CFG.from_string str
        | FromStdin ->
          pf "Please enter a CFG (end it with Ctrl+D):@.";
          CFG.from_channel stdin
      in
      let pda = Convert.cfg_to_pda cfg in
      pf "I have converted it to the following PDA:@\n%a@?" PDA.pp pda;
      pda
    )

let () = pf "@\nI have got %d PDAs to work with.@." (List.length pdas)

let alphabet =
  List.fold_left (fun alphabet pda -> PDA.alphabet pda @ alphabet) [] pdas
  |> List.sort_uniq compare

let () = pf "Their smallest common alphabet has %d letters and is: %s.@."
    (List.length alphabet) (String.concat ", " alphabet)

let () = pf "I shall test all the given PDAs on all the possible words by increasing length.
I will print the first %d accepted words.
I will stop as soon as I find a word that differenciates these PDAs.
I will not stop otherwise until I am killed (with Ctrl+C).@."
    !nb_words

let pp_word fmt = function
  | [] -> Format.pp_print_string fmt "λ"
  | word -> Format.(pp_print_list ~pp_sep:(fun _fmt () -> ()) pp_print_string) fmt word

let nb_printed_words = ref 0

let rec test_all_words length words_and_confs =
  pf "\r[%d] @?" length;

  (* Check that all PDA configurations agree on all the words. If they agree and
     accept, print the word. If they disagree, report and exit. *)
  List.iter
    (fun (word, confs) ->
       let acceptance = List.map PDA.Runner.accepting confs in
       if List.for_all not acceptance then
         ()
       else if List.for_all Fun.id acceptance then
         (
           if !nb_printed_words < !nb_words then
             (
               incr nb_printed_words;
               pf "%a" pp_word (List.rev word);
               if !nb_printed_words = !nb_words then
                 (
                   pf " (last printed one)";
                   if List.length pdas <= 1 then
                     (
                       pf "@.";
                       exit 0
                     )
                 );
               pf "@\n\r[%d] @?" length
             )
         )
       else
         (
           pf "The word %a differentiates these PDAs.@." pp_word (List.rev word);
           let acceptance = List.mapi (fun i b -> (i, b)) acceptance in
           let (accept, reject) = List.partition snd acceptance in
           let accept = accept |> List.map fst |> List.map soi |> String.concat ", " in
           let reject = reject |> List.map fst |> List.map soi |> String.concat ", " in
           pf "PDA/s %s accept it.@\nPDA/s %s reject it." accept reject;
           exit 1
         )
    )
    words_and_confs;

  let next_words_and_confs =
    words_and_confs |> List.concat_map @@ fun (word, confs) ->
    alphabet |> List.map @@ fun a ->
    (a :: word, List.map (PDA.Runner.step_letter a) confs)
  in

  test_all_words (length + 1) next_words_and_confs

let () = test_all_words 0 [([], List.map PDA.Runner.initial pdas)]
