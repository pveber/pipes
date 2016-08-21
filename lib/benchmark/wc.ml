open Core.Std
open CFStream
open Core_bench.Std

let random_char _ =
  Char.of_int_exn (65 + Random.int 52)

let text_generator n =
  let buf = Buffer.create 251 in
  let add_word () =
    String.init (Random.int 10 + 2) ~f:random_char
    |> Buffer.add_string buf
  in
  let rec loop = function
    | 0 -> ()
    | n ->
      match Random.int 10 with
      | 0 ->
        Buffer.add_char buf '\n' ;
        add_word () ;
        loop (n - 1)
      | _ ->
        Buffer.add_char buf ' ' ;
        add_word () ;
        loop n
  in
  add_word () ;
  loop n ;
  Buffer.contents buf

let line_wc s =
  let rec loop accu pos =
    match String.rfindi s ~pos ~f:(fun _ -> ( = ) ' ') with
    | None -> accu
    | Some pos' -> loop (accu + 1) pos'
  in
  loop 0 0

let simple_implementation t () =
  let lines = String.split ~on:'\n' t in
  let wc_per_line = List.map lines ~f:line_wc in
  List.length lines,
  List.fold_left wc_per_line ~init:0 ~f:( + )

let lines_step_implementation t () =
  let open Pipes_parsers in
  let _, lines = Lines.(step initial_state (Some t)) in
  let wc_per_line = List.map lines ~f:(fun s -> line_wc (s : Line.t :> string)) in
  List.length lines,
  List.fold_left wc_per_line ~init:0 ~f:( + )


let big_text = text_generator 1_000


let command =
  Bench.make_command [
    Bench.Test.create ~name:"simple" (simple_implementation big_text) ;
    Bench.Test.create ~name:"lines_step" (lines_step_implementation big_text) ;
  ]

