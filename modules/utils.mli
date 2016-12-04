(*************************** Miscellaneous Helpers ****************************)
(******************************************************************************)

(* returns true if the path contains an initialized Cml repo,
 * otherwise returns false *)
val cml_initialized: string -> bool

(* returns true if the current directory (or parent) is an initialized Cml repo,
 * otherwise returns false *)
val cml_initialized_r : string -> bool

(* returns whether the given argument is a flag (if arg is of the form
 * dash [-] followed by any number of characters > 0) *)
val arg_is_flag : string -> bool

(* precondition: [arg] is a flag.
 * postcondition: returns the list of flags from the argument.
 * example: [get_flags_from_arg "-hi" ~ ["h"; "i"]]
 * example: [get_flags_from_arg "--hi" ~ ["hi"]] *)
val get_flags_from_arg : string -> string list

(******************************** User Info ***********************************)
(******************************************************************************)

(* fetch the user info (username) *)
val get_user_info: unit -> string

(* set the user info (username) *)
val set_user_info: string -> unit
