open Core_kernel.Std

type state =
  | Current_line of Line.t
  | Finished

let initial_state = Current_line Line.empty

let step st input = match input, st with
  | _, Finished -> (Finished, [ Line.empty ])
  | None, Current_line l -> (Finished, [ l ])
  | Some sentence, Current_line l ->
    match Line.rightmost sentence with
    | None, rml -> Current_line (Line.append l rml), [ Line.empty ]
    | Some b, a ->
      match Line.make b with
      | [] -> assert false (* [Line.make] never returns an empty list *)
      | h :: t -> Current_line a, (Line.append l h) :: t

let reader = Pipe.loop step initial_state
