open Core.Std
open CFStream
open Core_bench.Std

let l = List.init 1_000 ~f:ident
let s = List.sum (module Int) ~f:ident l

let list_int_sum () =
  let n = List.fold l ~init:0 ~f:( + ) in
  assert (n = s)

let stream_int_sum () =
  let n =
    Stream.of_list l
    |> Stream.fold ~init:0 ~f:( + )
  in
  assert (n = s)

let pipe_int_sum () =
  let open Pipe in
  let n =
    run (from_list l $$ fold 0 ( + ))
  in
  assert (n = s)

let command =
  Bench.make_command [
    Bench.Test.create ~name:"list" list_int_sum ;
    Bench.Test.create ~name:"stream" stream_int_sum ;
    Bench.Test.create ~name:"pipe" pipe_int_sum ;
  ]

