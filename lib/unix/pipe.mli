include Pipes.Pipe.S with type 'a monad = 'a

val from_file : ?buffer_size:int -> string -> bytes source

val to_file : string -> string sink
