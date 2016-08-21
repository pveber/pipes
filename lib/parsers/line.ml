open Core_kernel.Std

type t = string

let empty = ""

let make x = String.split x ~on:'\n'

let rightmost x =
  match String.rsplit2 x ~on:'\n' with
  | None -> (None, x)
  | Some (b, a) -> (Some b, a)

let append x y = x ^ y
