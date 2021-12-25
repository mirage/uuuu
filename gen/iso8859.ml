type code = int
type name = string
type map  = code * Uchar.t * name option

module Map = Map.Make(struct type t = code let compare = compare end)

let extract source =
  let _, maps = List.partition (function `Comment _ -> true | _ -> false) source in
  let res = List.fold_left (fun map -> function
      | `Bind_with_name (a, b, name) -> Map.add a (b, Some name) map
      | `Bind (a, b) -> Map.add a (b, None) map
      | _ -> assert false) Map.empty maps in
  Ok res
