module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

type void

module type S = sig
  type 'a monad

  type 'a thunk = unit -> 'a

  type finalizer = (unit -> unit monad) option

  type ('i, 'o, 'r) t =
    | Has_output of 'o * ('i, 'o, 'r) t thunk * finalizer
    | Needs_input of ('i option -> ('i, 'o, 'r) t)
    | Done of 'r
    | PipeM of ('i, 'o, 'r) t monad thunk

  val return : 'r -> (_, _, 'r) t
  val bind : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t

  module Monad_infix : sig
    val ( >>= ) : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t
  end

  val await : unit -> ('a, _, 'a option) t
  val yield : 'o -> (_, 'o, unit) t
  val compose : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
  val ( $$ ) : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
  val run : (void, void, 'r) t -> 'r monad

  val bracket :
    (unit -> 'a monad) ->
    ('a -> unit monad) ->
    ('a -> ('i, 'o, 'r) t) ->
    ('i, 'o, 'r) t

  val fold : 'r -> ('i -> 'r -> 'r) -> ('i, void, 'r) t
  val map : ('i -> 'o) -> ('i, 'o, unit) t

  val from_list : 'a list -> (void, 'a, unit) t

  val loop : ('a -> 'b option -> 'a * 'c list) -> 'a -> ('b, 'c, unit) t

end

module Make(M : Monad) : S with type 'a monad = 'a M.t
