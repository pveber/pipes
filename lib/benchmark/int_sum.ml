open Core
open CFStream
open Core_bench.Std

let l = List.init 1_000 ~f:ident
let s = List.sum (module Int) ~f:succ l

let list_int_sum () =
  let n =
    List.map l ~f:succ
    |> List.fold ~init:0 ~f:( + ) in
  assert (n = s)

let stream_int_sum () =
  let n =
    Stream.of_list l
    |> Stream.map ~f:succ
    |> Stream.fold ~init:0 ~f:( + )
  in
  assert (n = s)

let pipe_int_sum () =
  let open Pipes_unix.Pipe in
  let n =
    run (
      from_list l
      $$ map succ
      $$ fold 0 ( + )
    )
  in
  assert (n = s)

let codensity_pipe_int_sum () =
  let open Codensity_pipe in
  let n =
    run (
      from_list l
      $$ map succ
      $$ fold 0 ( + )
    )
  in
  assert (n = s)

let command =
  Bench.make_command [
    Bench.Test.create ~name:"list" list_int_sum ;
    Bench.Test.create ~name:"stream" stream_int_sum ;
    Bench.Test.create ~name:"pipe" pipe_int_sum ;
    Bench.Test.create ~name:"codensity_pipe" codensity_pipe_int_sum ;
  ]

