let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let is_white = function
  | ' ' | '\t' .. '\r' -> true | _ -> false

(* (c) @dbuenzli *)
let trim s =
  let drop = is_white in
  let len = String.length s in
  let max_idx = len - 1 in
  if len = 0 then s else
    let rec left_pos i =
      if i > max_idx then len else
      if drop s.[i] then left_pos (i + 1) else i in
    let rec right_pos i =
      if i < 0 then 0 else
      if drop s.[i] then right_pos (i - 1) else (i + 1) in
    let left = left_pos 0 in
    if left = len then "" else
      let right = right_pos max_idx in
      if left = 0 && right = len then s else
        String.sub s left (right - left)

let add_sub str ~start ~stop acc =
  if start = stop
  then acc else String.sub str start (stop - start) :: acc

let head str =
  if String.length str > 0
  then Some str.[0]
  else None

let tail str =
  let len = String.length str in
  if len > 0
  then String.sub str 1 (len - 1)
  else ""

let cuts ~sep s =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "Invalid empty separator" else
  let s_len = String.length s in
  let max_sep_idx = sep_len - 1 in
  let max_s_idx = s_len - sep_len in
  let rec check_sep start i k acc =
    if k > max_sep_idx then
      let new_start = i + sep_len in
      scan new_start new_start (add_sub s ~start ~stop:i acc)
    else
      if String.get s (i + k) = String.get sep k
      then check_sep start i (k + 1) acc
      else scan start (i + 1) acc
  and scan start i acc =
    if i > max_s_idx then
      if start = 0 then (if s_len = 0 then [] else [s]) else
      List.rev (add_sub s ~start ~stop:s_len acc)
    else
      if String.get s i = String.get sep 0
      then check_sep start i 1 acc
      else scan start (i + 1) acc
  in
  scan 0 0 []

let number_of_hex str =
  try Ok (int_of_string str) with
  | _exn -> error_msgf "Invalid number: %S" str

let name_of_comment str = match head str with
  | Some '#' -> Ok (trim (tail str))
  | Some _ | None -> error_msgf "Invalid comment: %S" str

let with_ic path f a =
  try
    let ic = open_in path in
    let rs = f ic a in
    close_in ic ; Ok rs
  with _ -> error_msgf "Invalid filename: %s" path

let with_oc path f a =
  try
    let oc = open_out path in
    let rs = f oc a in
    close_out oc ; rs
  with _ -> error_msgf "Invalid filename: %s" path

let parse_line line = match cuts ~sep:"\t" line with
  | a :: b :: r ->
    let name = String.concat "\t" r in
    ( match number_of_hex a, number_of_hex b, name_of_comment name with
    | Ok a, Ok b, Ok name -> Ok (`Bind_with_name (a, b, name))
    | Ok a, Ok b, _ -> Ok (`Bind (a, b))
    | _ -> error_msgf "Invalid line: %S" line )
  | _ -> error_msgf "Invalid line: %S" line

let rec parser ic (n, acc) = match input_line ic with
  | line ->
    ( match head line with
    | None -> parser ic (succ n, acc)
    | Some '#' ->  parser ic (succ n, `Comment (tail line) :: acc)
    | Some _ ->
      ( match parse_line line with
      | Ok v -> parser ic (succ n, v :: acc)
      | Error (`Msg err) ->
        Format.eprintf "Error l.%d: %s.\n%!" n err ;
        parser ic (succ n, acc) ) )
  | exception End_of_file -> List.rev acc

let of_file path = with_ic path parser (0, [])

module Map = Iso8859.Map

let pp_list ~sep:pp_sep pp_data ppf lst =
  let rec go = function
    | [] -> ()
    | [ x ] -> Format.fprintf ppf "%a" pp_data x
    | x :: r -> Format.fprintf ppf "%a%a@ " pp_data x pp_sep () ; go r in
  go lst

let pp_array ppf lst =
  let sep ppf () = Format.pp_print_string ppf ";" in
  Format.fprintf ppf "[|@[<hov>@ %a@ @]|]" (pp_list ~sep Format.pp_print_int) lst

let produce oc database =
  let ppf = Format.formatter_of_out_channel oc in
  let res = Array.init 256 (fun idx -> match Map.find idx database with
      | (cp, _) -> cp
      | exception Not_found -> (-1)) |> Array.to_list in
  Format.fprintf ppf "let map = %a\n%!" pp_array res; Ok ()

let ( >>= ) x f = match x with
  | Ok x -> f x
  | Error err -> Error err

let parse source destination =
  of_file source >>= fun src ->
  Iso8859.extract src >>= fun maps ->
  with_oc destination produce maps

let exit_success = 0
let exit_failure = 1

let report = function
  | Ok () -> exit exit_success
  | Error (`Msg err) -> Format.eprintf "%s: %s.\n%!" Sys.argv.(0) err

let () = match Sys.argv with
  | [| _; source; destination; |] ->
    if Sys.file_exists source
    then report (parse source destination)
    else ( Format.eprintf "%s source destination\n%!" Sys.argv.(0) ; exit exit_failure )
  | _ ->
    Format.eprintf "%s source destination\n%!" Sys.argv.(0) ; exit exit_failure
