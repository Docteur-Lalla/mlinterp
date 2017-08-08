open Batteries
open BatOptParse

exception BadNumberOfFilesException of int
exception NoFilenameException

let optparser = OptParser.make
  ~prog: "mlinterp"
  ~usage: "%prog - Interpreter of OCaml code"
  ~version: "0.1"
  ()

(* This function parses the program's arguments and get the filename if any.
 * If no filename has been sent has argument, a NoFilenameException is raised.
 * If several filenames have been sent, a BadNumberOfFilesException is raised. *)
let get_filename () =
  let files = OptParser.parse_argv optparser in
  match files with
  | [h] -> h
  | [] -> raise NoFilenameException
  | _ -> raise @@ BadNumberOfFilesException (BatList.length files)

let get_contents filename =
  let file_in_channel = Pervasives.open_in filename in
  let file_input = BatIO.input_channel
    ~autoclose: true
    ~cleanup: true
    file_in_channel in
  BatIO.read_all file_input

(** This function reads lines until ";;" appears in the command typed. *)
let read_toplevel_phrase () =
  let cmd = ref "" in
  let contains_end str =
    try ignore @@ BatString.find str ";;" ; true
    with _ -> false in
  while not (contains_end !cmd) do
    let line = read_line () in
    cmd := !cmd ^ "\n" ^ line ;
    if not (contains_end !cmd) then
      print_string "> "
  done ;
  !cmd

(** Read-Eval-Print Loop.
 * It reads a toplevel phrase, evaluates it and prints the result.
 * This function loops until the input is "#quit ;;". *)
let rec repl state ctx =
  try
    print_string "# " ;
    let cmd = read_toplevel_phrase () in
    let lexbuf = Lexing.from_string cmd in
    let toplevel_phrase = Parse.toplevel_phrase lexbuf in
    match toplevel_phrase with
    | Parsetree.Ptop_dir ("quit", _) -> ()
    | Parsetree.Ptop_dir (s, _) -> print_endline s ; repl state ctx
    | Parsetree.Ptop_def strct ->
      let ctx'' = try
        let (ctx', value) = Interpreter.run_structure state ctx strct in
        ValueUtils.print_value state value ;
        ctx'
      with
      | Interpreter.NotImplemented -> print_endline "Feature not implemented yet." ; ctx
      | Interpreter.MatchFailureException -> print_endline "Matching failure" ; ctx
      | Interpreter.NotSupportedException s -> print_endline @@ "Feature \"" ^ s ^ "\" is not supported." ; ctx
      | Interpreter.NotFunctionException v ->
        print_endline @@ "Value " ^ ValueUtils.string_of_value state v ^ " is not a function." ; ctx
      | Value.TypeError -> print_endline "Type error." ; ctx
      | Interpreter.AssertFalseException -> print_endline "Assertion evaluated to false." ; ctx in
      repl state ctx''
  with
  | e -> raise e

(** Entry-point of the interpreter. *)
let () =
  try
    try
      let filename = get_filename () in
      let state = State.empty () in
      get_contents filename
      |> Lexing.from_string
      |> Parse.implementation
      |> Interpreter.run_structure state Context.empty
      |> snd
      |> ValueUtils.print_value state
    with
    | NoFilenameException -> repl (State.empty ()) Context.empty
  with
  | BadNumberOfFilesException n ->
    BatPrintf.printf "Error: expected one input file, got %n.\n" n
