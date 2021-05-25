open Ext
open Toytomata
open Common

let pf = Format.printf
let spf = Format.sprintf

let soi = string_of_int

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

let languages =
  all_inputs () |> List.mapi @@ fun i input ->
  pf "@\nLanguage #%d:@." (i + 1);
  match input.source with
  | FromFile fname ->
    pf "Reading from file \"%s\".@." fname;
    Language.from_file_exn fname
  | FromString str ->
    pf "Reading from string \"%s\".@." str;
    Language.from_string_exn str
  | FromStdin ->
    pf "Please enter a PDA (end it with Ctrl+D):@.";
    Language.from_channel_exn stdin

let () = pf "@\nI have got %d languages to work with.@." (List.length languages)

let alphabet =
  List.map Language.alphabet languages
  |> List.fold_left Alphabet.union Alphabet.empty

let () = pf "Their smallest common alphabet has %d letters and is: %a.@."
    (Alphabet.length alphabet) (Alphabet.pp ", ") alphabet

let () = pf "I shall test all the given languages on all the possible words by increasing length.
I will print the first %d accepted words.
I will stop as soon as I find a word that differenciates these PDAs.
I will not stop otherwise until I am killed (with Ctrl+C).@."
    !nb_words

let nb_printed_words = ref 0

let accepts = List.map Language.accepts languages

let () =
  Word.all_words alphabet
  |> Seq.iter @@ fun word ->
  let acceptance = List.map ((|>) word) accepts in
  if List.for_all (function `False -> true | _ -> false) acceptance then
    ()
  else if List.for_all (function `True -> true | _ -> false) acceptance then
    (
      if !nb_printed_words < !nb_words then
        (
          incr nb_printed_words;
          pf "%a" Word.pp word;
          if !nb_printed_words = !nb_words then
            (
              pf " (last printed one)";
              if List.length languages <= 1 then
                (
                  pf "@.";
                  exit 0
                )
            );
          pf "@\n\r[%d] @?" (Word.length word)
        )
    )
  else
    (
      pf "The word %a differentiates these PDAs.@." Word.pp word;
      List.iteri
        (fun i acceptance ->
           pf "Language %d %s@." (i+1)
             (match acceptance with
              | `True -> "accepts it"
              | `False -> "rejects it"
              | `DontKnow _ -> "does not know"))
        acceptance;
      exit 1
    )
