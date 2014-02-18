type 'a t = (Syntax.var * (Syntax.varid * 'a)) list

let empty = []
let rec identify x = function
  | [] -> raise Not_found
  | (x', (i, _)) :: _ when x = x' -> i
  | _ :: env -> identify x env
let rec lookup x = function
  | [] -> raise Not_found
  | (x', (_, a)) :: _ when x = x' -> a
  | _ :: env -> lookup x env
let rec lookup_by_id i = function
  | [] -> raise Not_found
  | (_, (i', a)) :: _ when i = i' -> a
  | _ :: env -> lookup_by_id i env
let rec bound x = function
  | [] -> false
  | (x', _) :: _ when x = x' -> true
  | _ :: env -> bound x env
let extend (x, i) a env = (x, (i, a)) :: env
let rec remove x = function
  | [] -> []
  | (x', _) :: env when x = x' -> env
  | _ :: env -> remove x env
let rec remove_by_id i = function
  | [] -> []
  | (_, (i', _)) :: env when i = i' -> env
  | p :: env -> p :: (remove_by_id i env)
let rec remove_if p = function
  | [] -> []
  | (x, (i, _)) :: env when p x i -> env
  | b :: env -> b :: remove_if p env
let rec update x a = function
  | [] -> raise Not_found
  | (x', (i, _)) :: env when x = x' -> (x, (i, a)) :: env
  | p :: env -> p :: (update x a env)
let rec update_by_id i a = function
  | [] -> raise Not_found
  | (x, (i', _)) :: env when i = i' -> (x, (i, a)) :: env
  | p :: env -> p :: (update_by_id i a env)
let rec remove_all x = function
  | [] -> []
  | (x', _) :: env when x = x' -> remove_all x env
  | _ :: env -> remove_all x env
let rec names = function
  | [] -> []
  | (x, _) :: env -> x :: names (remove_all x env)
let ids env = List.map (fun (_, (i, _)) -> i) env
let unname env = List.map (fun (_, p) -> p) env

let rec fold f z = function
  | [] -> z
  | (x, (i, a)) :: env -> f (x, i) a (fold f z env)
let rec iter f = function
  | [] -> ()
  | (x, (i, a)) :: env -> f x i a; iter f env
