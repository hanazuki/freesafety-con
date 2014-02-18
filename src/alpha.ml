open Syntax



let rec identify_comm vmap env = function
  | Skip -> Skip, vmap
  | Assign (x, y) ->
    let x = 
  | Sequence of 'a comm * 'a comm
  | Free of 'a
  | LetMalloc of 'a * 'a comm
  | LetNull of 'a * 'a comm
  | LetVar of 'a * 'a * 'a comm
  | LetDeref of 'a * 'a * 'a comm
  | IfNull of 'a *'a  comm * 'a comm
  | FunCall of 'a * 'a list
  | AssertEqVar of 'a * 'a
  | AssertEqDeref of 'a * 'a
  | LetNewlock of 'a * 'a comm
  | Freelock of 'a
  | Acquire of 'a
  | Release of 'a
  | LetFork of 'a * 'a comm * 'a comm
  | Wait of 'a

type 'a defn = 'a * ('a list * 'a comm)

type 'a prog = 'a defn list * 'a comm
