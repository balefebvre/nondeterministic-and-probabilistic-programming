(** An implementation of first-class stores *)

module IMap =
  Map.Make(struct type t = int let compare = Pervasives.compare end)

type t = exn IMap.t

type 'a loc = { index: int; make: 'a -> exn; get: exn -> 'a option }

let last_index = ref 0

let newloc (type a) () =
  let module M = struct exception E of a end in
  incr last_index;
  { index = !last_index;
    make = (fun x -> M.E x);
    get = (function M.E x -> Some x | _ -> None) }

let string_of_loc l = string_of_int l.index

let empty = IMap.empty

let get st l =
  try
    l.get (IMap.find l.index st)
  with Not_found ->
    None

let put st l v =
  IMap.add l.index (l.make v) st

