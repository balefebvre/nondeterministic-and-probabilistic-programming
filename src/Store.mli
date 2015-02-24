(** First-class stores *)

type 'a loc
  (** The type of store locations that contain values of type ['a]. *)

val newloc: unit -> 'a loc
  (** Generate a fresh location of type ['a loc] *)
val string_of_loc: 'a loc -> string
  (** For debugging purposes: convert a location to a printable
  representation. *)

type t
  (** The type of first-class stores.  These are finite maps from
      locations to values.  A location of type ['a loc] is mapped
      to a value of type ['a]. *)

val empty: t
  (** The empty, initial store. *)

val get: t -> 'a loc -> 'a option
  (** [get st l] returns the value associated with location [l] in store [st],
      if any, or [None] if no such value exists. *)

val put: t -> 'a loc -> 'a -> t
  (** [put st l v] associates value [v] to location [l] in store [st],
      and returns the updated store. *)
