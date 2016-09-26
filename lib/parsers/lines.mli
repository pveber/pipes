open Pipes_pure

type state

val initial_state : state

val step : state -> string option -> state * Line.t list
