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
    cmd := !cmd ^ "\n" ^ line
  done ;
  !cmd

(** Read-Eval-Print Loop.
 * It reads a toplevel phrase, evaluates it and prints the result.
 * This function loops until the input is "#quit ;;". *)
let rec repl () =
  try
    let cmd = read_toplevel_phrase () in
    let lexbuf = Lexing.from_string cmd in
    let toplevel_phrase = Parse.toplevel_phrase lexbuf in
    match toplevel_phrase with
    | Parsetree.Ptop_dir ("quit", _) -> ()
    | Parsetree.Ptop_dir (s, _) -> print_endline s ; repl ()
    | Parsetree.Ptop_def strct ->
      Interpreter.run_structure strct
      |> Value.print_value
      |> repl
  with
  | e -> raise e

(** Entry-point of the interpreter. *)
let () =
  try
    try
      let filename = get_filename () in
      get_contents filename
      |> Lexing.from_string
      |> Parse.implementation
      |> Interpreter.run_structure
      |> Value.print_value
    with
    | NoFilenameException -> repl ()
  with
  | BadNumberOfFilesException n ->
    BatPrintf.printf "Error: expected one input file, got %n.\n" n
