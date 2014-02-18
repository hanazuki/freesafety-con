type var = string
type varid = int

let varid =
  let n = ref 0 in
  fun (x: var) ->
    let i = !n in
    incr n; (x, i)

let tmpvar =
  let n = ref 0 in
  fun () ->
    let i = !n in
    incr n; "__tmp" ^ string_of_int i

type comm =
  | Skip
  | Assign of var * var
  | Sequence of comm * comm
  | Free of var
  | LetMalloc of (var * varid) * comm
  | LetNull of (var * varid) * comm
  | LetVar of (var * varid) * var * comm
  | LetDeref of (var * varid) * var * comm
  | IfNull of var * comm * comm
  | FunCall of var * var list
  | AssertEqVar of var * var
  | AssertEqDeref of var * var
  | LetNewlock of (var * varid) * comm
  | Freelock of var
  | Acquire of var
  | Release of var
  | LetFork of (var * varid) * comm * comm
  | Wait of var

type defn = (var * varid) * ((var * varid) list * comm)

type prog = defn list * comm

let rec freevars = function
  | Skip -> ListSet.empty
  | Assign (x, y) -> ListSet.fromList [x; y]
  | Sequence (s, t) -> ListSet.union (freevars s) (freevars t)
  | Free x -> ListSet.singleton x
  | LetMalloc ((x, _), s) -> ListSet.remove x (freevars s)
  | LetNull ((x, _), s) -> ListSet.remove x (freevars s)
  | LetVar ((x, _), y, s) -> ListSet.add y (ListSet.remove x (freevars s))
  | LetDeref ((x, _), y, s) -> ListSet.add y (ListSet.remove x (freevars s))
  | IfNull (x, s, t) -> ListSet.add x (ListSet.union (freevars s) (freevars t))
  | FunCall (_f, xs) -> ListSet.fromList xs
  | AssertEqVar (x, y) -> ListSet.fromList [x; y]
  | AssertEqDeref (x, y) -> ListSet.fromList [x; y]
  | LetNewlock ((x, _), s) -> ListSet.remove x (freevars s)
  | Freelock x -> ListSet.singleton x
  | Acquire x -> ListSet.singleton x
  | Release x -> ListSet.singleton x
  | LetFork ((x, _), s, t) -> ListSet.union (freevars s) (ListSet.remove x (freevars t))
  | Wait x -> ListSet.singleton x
