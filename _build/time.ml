(* returns a timestamp in the format [hr:min:sec] *)
let time (h : int) (m : int) (s : int) : string =
  (string_of_int h) ^ ":" ^ (string_of_int m) ^ ":" ^ (string_of_int s)

(* returns the day of the week abbreviation *)
let day (n : int) : string = match n with
  | 0 -> "Sun" | 1 -> "Mon" | 2 -> "Tue" | 3 -> "Wed"
  | 4 -> "Thu" | 5 -> "Fri" | 6 -> "Sat" | _ -> "Michael"

(* returns the month abbreviation *)
let month (n : int) : string = match n with
  | 0 -> "Jan" | 1 -> "Feb" | 2  -> "Mar" | 3  -> "Apr"
  | 4 -> "May" | 5 -> "Jun" | 6  -> "Jul" | 7  -> "Aug"
  | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec"
  | _ -> "Clarkson"

(* returns the current year *)
let year (n : int) : string = string_of_int (n + 1900)

(* returns a formatted timestamp string for cml commits *)
let get_time (tm : Unix.tm) : string =
  let time = time tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
  let date = string_of_int tm.Unix.tm_mday in
  let day  = day tm.Unix.tm_wday in
  let mon  = month tm.Unix.tm_mon in
  let year = year tm.Unix.tm_year in
    (day ^ " " ^ mon ^ " " ^ date ^ " " ^ time ^ " " ^ year)

