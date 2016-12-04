open Unix
open Universal

(************************** Miscellaneous Helpers *****************************)
(******************************************************************************)

(* returns true if the path contains an initialized Cml repo,
 * otherwise returns false *)
let cml_initialized (path : string) : bool =
  Sys.file_exists (path^".cml")

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise returns false *)
let cml_initialized_r (path : string) : bool =
  try match Filesystem.cml_path path with
    | Some _ -> true
    | None -> false
  with
  | Fatal _ -> false

(* returns whether the given argument is a flag (if arg is of the form
 * dash [-] followed by any number of characters > 0) *)
let arg_is_flag (arg : string) : bool =
  let r = Str.regexp "^-.*" in
  Str.string_match r arg 0

(* precondition: [arg] is a flag.
 * postcondition: returns the list of flags from the argument.
 * example: [get_flags_from_arg "-hi" ~ ["h"; "i"]]
 * example: [get_flags_from_arg "--hi" ~ ["hi"]] *)
let get_flags_from_arg (arg : string) : string list =
  let r_double_dash = Str.regexp "^--" in
  let r_single_dash = Str.regexp "^-" in
  if Str.string_match r_double_dash arg 0 then
    [Str.replace_first r_double_dash "" arg]
  else
    Str.replace_first r_single_dash "" arg |> Str.split (Str.regexp "")

(* fetch the user info (username) *)
let get_user_info () : string =
  try
    let ch = open_in ".cml/config" in
    let raw = input_line ch in
    let split = (String.index raw ' ') + 1 in
    close_in ch; String.sub raw split (String.length raw - split)
  with
  | Sys_error _ -> raise (Fatal "username not set, set with [--user <name>]")

(* set the user info (username) *)
let set_user_info (name : string) : unit =
  let ch = open_out ".cml/config" in
  output_string ch ("user: "^name^"\n"); close_out ch
