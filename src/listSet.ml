type 'a t = 'a list

let empty = []
let singleton x = [x]
let mem = List.mem
let add x s = if List.mem x s then s else x :: s
let union s t = List.fold_left (fun r x -> add x r) t s
let inter s t = List.fold_left (fun r x -> if mem x t then add x r else r) [] s
let remove x = function
  | [] -> []
  | y :: ys when x = y -> ys
  | ys -> ys
let fromList l = List.fold_left (fun r x -> add x r) [] l
let subset s t = List.for_all (fun x -> mem x t) s
