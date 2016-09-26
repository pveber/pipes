include Pipes_pure.Pipe.S with type 'a monad = 'a

val from_file : string -> (void, string, unit) t

val to_file : string -> (string, void, unit) t
