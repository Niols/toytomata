open Common

(** {2 AST} *)

include AST
type t = AST.pda

(** {2 CST Parsing & Printing} *)

let cst_from_lexbuf = Parser.entrypoint Lexer.read

let cst_from_channel ichan = cst_from_lexbuf (Lexing.from_channel ichan)
let cst_from_string str = cst_from_lexbuf (Lexing.from_string str)

let cst_from_file fname =
  let ichan = open_in fname in
  let pda = cst_from_channel ichan in
  close_in ichan;
  pda

let pp_cst = Printer.pp_pda'

let cst_to_string = Format.asprintf "%a" pp_cst

let cst_to_channel ochan pda =
  let fmt = Format.formatter_of_out_channel ochan in
  Printer.pp_pda' fmt pda

let cst_to_file fname pda =
  let ochan = open_out fname in
  cst_to_channel ochan pda;
  close_out ochan

let cst_to_ast = CST_to_AST.pda'__to__pda
let ast_to_cst = AST_to_CST.pda__to__pda'

(** {2 AST Parsing & Printing} *)

let from_channel ichan = cst_from_channel ichan |> cst_to_ast
let from_string str = cst_from_string str |> cst_to_ast
let from_file fname = cst_from_file fname |> cst_to_ast

let pp fmt g = g |> ast_to_cst |> pp_cst fmt
let to_channel ochan g = g |> ast_to_cst |> cst_to_channel ochan
let to_string g = g |> ast_to_cst |> cst_to_string
let to_file fname g = g |> ast_to_cst |> cst_to_file fname

(** {2 Rest} *)

let alphabet pda =
  pda.transitions
  |> List.filter_map (fun ((_, lo, _), (_, _)) -> lo)
  |> List.sort_uniq compare

let push_maybe stack = function
  | None -> stack
  | Some symbol -> Stack.push symbol stack

let one_step (pda : pda) ((state, word, stack) : configuration) =
  List.concat_map
    (fun ((state', letter', symbol'), (new_state', push_symbol')) ->
       try
         assert (state = state');
         let new_word =
           match letter', word with
           | None, word -> word
           | Some letter', letter :: word when letter = letter' -> word
           | _ -> assert false
         in
         match symbol', Stack.pop_opt stack with
         | None, _ ->
           [(new_state', new_word, push_maybe stack push_symbol')]

         | Some symbol', Some (stack_top, stack) when symbol' = stack_top ->
           [(new_state', new_word, push_maybe stack push_symbol')]

         | _ ->
           []
       with
         Assert_failure _ -> [])
    pda.transitions

let rec all_steps pda ((state, word, stack) as conf) =
  if word = [] && List.mem state pda.finals && Stack.is_empty stack then
    [conf]
  else
    one_step pda conf
    |> List.concat_map (all_steps pda)

let accepts pda word =
  List.concat_map (all_steps pda) (List.map (fun q -> (q, word, Stack.empty)) pda.initials) <> []
