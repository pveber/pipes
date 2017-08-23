open Pipes

type void = Pipe.void

module type S = sig
  type 'a monad

  type ('i, 'o, 'r) t

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

module Make(M : Pipe.Monad) = struct

  let ( >>= ) x f = M.bind x f

  module P = Pipe.Make(M)

  type 'a monad = 'a M.t

  type ('i, 'o, 'r) t = {
    unPipe : 'x. ('r -> ('i, 'o, 'x) P.t) -> ('i, 'o, 'x) P.t
  }

  let return : 'r -> ('i, 'o, 'r) t =
    fun r ->
      { unPipe = fun f -> f r }

  let bind : ('i, 'o, 'a) t -> ('a -> ('i, 'o, 'b) t) -> ('i, 'o, 'b) t
      = fun x f ->
        {
          unPipe = fun h ->
            x.unPipe (fun a -> (f a).unPipe h)
        }

  module Monad_infix = struct
    let ( >>= ) x f = bind x f
  end

  let await : unit -> ('a, _, 'a option) t =
    fun () ->
      {
        unPipe = fun f -> P.Needs_input f
      }

  let yield
    : 'o -> (_, 'o, unit) t
    = fun o ->
      {
        unPipe = fun f -> P.Has_output (o, f, None)
      }

  let finalizer_compose f g =
    match f, g with
    | None, None -> None
    | Some f, None -> Some f
    | None, Some g -> Some g
    | Some f, Some g -> Some (fun () -> f () >>= g)

  let finalize = function
    | None -> M.return ()
    | Some f -> f ()

  let done_ x = P.Done x

  let compose
    : ('i, 'a, _) t -> ('a, 'o, 'r) t -> ('i, 'o, 'r) t
    = fun p q ->

      let open P in
      {
        unPipe = fun rest ->

          let rec go_right final left = function
            | Has_output (o, next, clean) ->
              let next () = go_right final left (next ()) in
              let clean = finalizer_compose clean final in
              Has_output (o, next, clean)

            | Needs_input f ->
              go_left f final left

            | Done r ->
              PipeM (fun () ->
                  finalize final >>= fun () ->
                  M.return (rest r)
                )

            | PipeM m ->
              PipeM (fun () ->
                  m () >>= fun p ->
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
              PipeM (fun () ->
                m () >>= fun p ->
                M.return (go_left next_right final p)
              )
          in
          go_right None (p.unPipe done_) (q.unPipe done_)
      }


    let ( $$ ) = compose

    let run x = P.run (x.unPipe done_)

    let rec fold init f =
      let open Monad_infix in
      await () >>= function
      | None -> return init
      | Some i ->
        fold (f i init) f


    let rec map f =
      let open Monad_infix in
      await () >>= function
      | None -> return ()
      | Some x ->
        yield (f x) >>= fun () -> map f

    let from_list l =
      let open Monad_infix in
      let rec loop = function
        | [] -> return ()
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

