# pipes
Stream programming in the style of Haskell's {pipe, conduit, iteratee}

* Examples
```
# open Pipes_pure.Pipe;;
# run (from_list [ 1 ; 2 ; 3 ] $$ fold 0 ( + ));;
- : int Pipes_pure.Pipe.monad = 6
```
