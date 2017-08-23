open Core
open Pipes_bench

let () =
  Command.(
    let whole_thing =
      group ~summary:"Benchmarks for pipes" [
        "int-sum", Int_sum.command ;
        "wc", Wc.command ;
      ] in
    run whole_thing
  )
