module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports

exception ParseError of Lexing.position * Lexing.position * string

let parse_error_fmt start end_ =
  Format.kasprintf @@ fun msg ->
  raise (ParseError (start, end_, msg))

let parse_error start end_ msg =
  parse_error_fmt start end_ "%s" msg

module type PARSER = sig
  type token

  val lexer_entrypoint : Lexing.lexbuf -> token

  module MenhirInterpreter : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token

  type output

  val entrypoint : Lexing.position -> output MenhirInterpreter.checkpoint
end

module Make (Parser : PARSER) = struct
  module I = Parser.MenhirInterpreter

  let show text positions =
    E.extract text positions
    |> E.sanitize
    |> E.compress
    |> E.shorten 20 (* max width 43 *)

  let env checkpoint =
    match checkpoint with
    | I.HandlingError env -> env
    | _ -> assert false

  let state checkpoint : int =
    match I.top (env checkpoint) with
    | Some (I.Element (s, _, _, _)) -> I.number s
    | None -> 0 (* FIXME *)

  let succeed v = Ok v

  let fail text buffer _checkpoint =
    let (start, end_) = E.last buffer in
    let indication = Format.sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
    Error (start, end_, indication)

  let from_string ?(filename="-") text =
    let lexbuf = L.init filename (Lexing.from_string text) in
    let supplier = I.lexer_lexbuf_to_supplier Parser.lexer_entrypoint lexbuf in
    let (buffer, supplier) = E.wrap_supplier supplier in
    let checkpoint = Parser.entrypoint lexbuf.lex_curr_p in
    I.loop_handle succeed (fail text buffer) supplier checkpoint

  let from_channel ?filename ichan =
    let text =
      let buflen = 1024 in
      let out = Buffer.create buflen in
      let buf = Bytes.create buflen in
      let rec read () =
        match input ichan buf 0 buflen with
        | 0 -> ()
        | n -> Buffer.add_subbytes out buf 0 n; read ()
      in
      read ();
      Buffer.contents out
    in
    from_string ?filename text

  let from_file fname =
    let ichan = open_in fname in
    let grammar = from_channel ~filename:fname ichan in
    close_in ichan;
    grammar

  let from_string_exn ?filename text =
    match from_string ?filename text with
    | Ok v -> v
    | Error (start, end_, msg) -> parse_error start end_ msg

  let from_channel_exn ?filename text =
    match from_channel ?filename text with
    | Ok v -> v
    | Error (start, end_, msg) -> parse_error start end_ msg

  let from_file_exn filename =
    match from_file filename with
    | Ok v -> v
    | Error (start, end_, msg) -> parse_error start end_ msg
end
