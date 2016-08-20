type void

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  type ('i, 'o, 'r) t
  type 'a monad

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

  val fold : 'r -> ('i -> 'r -> 'r) -> ('i, void, 'r) t
  val map : ('i -> 'o) -> ('i, 'o, unit) t
  val from_list : 'a list -> (void, 'a, unit) t
end

module Make(M : Monad) = struct
  let ( >>= ) x f = M.bind x f

  type 'a monad = 'a M.t

  type 'a thunk = unit -> 'a

  type finalizer = (unit -> unit M.t) option

  type ('i, 'o, 'r) t =
    | Has_output of 'o * ('i, 'o, 'r) t thunk * finalizer
    | Needs_input of ('i option -> ('i, 'o, 'r) t)
    | Done of 'r
    | PipeM of ('i, 'o, 'r) t M.t

  let return x = Done x

  let rec bind
  (*    : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t *)
    = fun x f ->
      match x with
      | Has_output (o, next, cleanup) ->
        Has_output (o, (fun () -> bind (next ()) f), cleanup)

      | Needs_input on_input ->
        Needs_input (fun i -> bind (on_input i) f)

      | Done r -> f r

      | PipeM m -> PipeM (m >>= fun p -> M.return (bind p f))

  module Monad_infix = struct
    let ( >>= ) x f = bind x f
  end

  let await () =
    Needs_input (fun i -> Done i)

  let yield x =
    Has_output (x, (fun () -> Done ()),  None)

  let finalizer_compose f g =
    match f, g with
    | None, None -> None
    | Some f, None -> Some f
    | None, Some g -> Some g
    | Some f, Some g -> Some (fun () -> f () >>= g)

  let finalize = function
    | None -> M.return ()
    | Some f -> f ()

  let compose p q =
    let rec go_right final left = function
      | Has_output (o, next, clean) ->
        let next () = go_right final left (next ()) in
        let clean = finalizer_compose clean final in
        Has_output (o, next, clean)

      | Needs_input f ->
        go_left f final left

      | Done r ->
        PipeM (
          finalize final >>= fun () ->
          M.return (Done r)
        )

      | PipeM m ->
        PipeM (
          m >>= fun p ->
          M.return (
            go_right final left p
          )
        )

    and go_left next_right final = function
      | Has_output (o, next, final') ->
        go_right final' (next ()) (next_right (Some o))

      | Needs_input f ->
        Needs_input (fun i -> go_left next_right final (f i))

      | Done r ->
        go_right None (Done r) (next_right None)

      | PipeM m ->
        PipeM (
          m >>= fun p ->
          M.return (go_left next_right final p)
        )
    in
    go_right None p q

  let ( $$ ) = compose

  let rec run = function
    | Done r -> M.return r
    | PipeM m -> m >>= run
    | Has_output _ -> assert false
    | Needs_input _ -> assert false


  let rec fold init f =
    Needs_input (function
        | None -> Done init
        | Some i -> fold (f i init) f
      )

  let rec map f =
    let open Monad_infix in
    await () >>= function
    | None -> return ()
    | Some x ->
      yield (f x) >>= fun () -> map f

  let from_list l =
    let open Monad_infix in
    let rec loop = function
      | [] -> Done ()
      | h :: t ->
        yield h >>= fun () ->
        loop t
    in
    loop l
end

include Make(struct
    type 'a t = 'a
    let return x = x
    let bind x f = f x
  end)
