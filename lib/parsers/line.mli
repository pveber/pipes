type t = private string

val empty : t

val make : string -> t list

val rightmost : string -> string option * t

val append : t -> t -> t
