module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

type void

module Make(M : Monad) :
sig
  type ('i, 'o, 'r) t
  val return : 'r -> (_, _, 'r) t
  val bind : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t
  val await : unit -> ('a, _, 'a option) t
  val yield : 'o -> (_, 'o, unit) t
  val compose : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
  val ( $$ ) : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
  val run : (unit, void, 'r) t -> 'r M.t

  val fold : 'r -> ('i -> 'r -> 'r) -> ('i, void, 'r) t
end
