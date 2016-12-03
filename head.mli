(**************************** HEAD Ptr Manipulation ***************************)
(******************************************************************************)

(* returns the current HEAD of the cml repository *)
val get_head: unit -> string

(* sets the HEAD of the cml repository *)
val set_head: string -> unit

(* sets the HEAD of the cml repository to a commit hash *)
val set_detached_head: string -> unit

(* returns the commit hash the head was detached at *)
val get_detached_head: unit -> string

(* returns true if repo currently is in detached head mode, else false *)
val detached_head: unit -> bool

(* returns correct head depending on detached_head mode *)
val get_head_safe : unit -> string
