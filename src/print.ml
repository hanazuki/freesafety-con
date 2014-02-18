let pp_paren b ff =
  if b then Format.fprintf ff "@[( @[<hv>%t@] )@]"
  else Format.fprintf ff "%t"

let rec pp_list delim pp ff = function
  | [] -> ()
  | [x] -> pp ff x
  | x :: xs ->
    Format.fprintf ff "%a%s@ %a"
      pp x delim (pp_list delim pp) xs

let pp_map fn pp ff x = pp ff (fn x)


(***** Syntax *)
open Syntax

let pp_var =
  Format.pp_print_string
let pp_var_with_id ff (x, i) =
  Format.fprintf ff "@[%s (*%d*)@]" x i

let pp_comm ff =
  let rec pp_comm p ff = function
    | Skip ->
      Format.fprintf ff "@[()@]"
    | Assign (x, y) ->
      Format.fprintf ff "@[<2>*%s <-@ %s@]" x y
    | Sequence (s, t) ->
      pp_paren (p > 1) ff
        (fun ff ->
          Format.fprintf ff "%a;@ %a"
            (pp_comm 1) s (pp_comm 0) t)
    | Free x ->
      Format.fprintf ff "@[free %s@]" x
    | LetMalloc (x, s) ->
      pp_paren (p > 0) ff
        (fun ff ->
          Format.fprintf ff "@[<v>@[<2>let %a =@ malloc ()@] in@ %a@]"
            pp_var_with_id x (pp_comm 0) s)
    | LetNull (x, s) ->
      pp_paren (p > 0) ff
        (fun ff ->
          Format.fprintf ff "@[<v>@[<2>let %a =@ null@] in@ %a@]"
            pp_var_with_id x (pp_comm 0) s)
    | LetVar (x, y, s) ->
      pp_paren (p > 0) ff
        (fun ff ->
          Format.fprintf ff "@[<v>@[<2>let %a =@ %a@] in@ %a@]"
            pp_var_with_id x pp_var y (pp_comm 0) s)
    | LetDeref (x, y, s) ->
      pp_paren (p > 0) ff
        (fun ff ->
          Format.fprintf ff "@[<v>@[<2>let %a =@ *%a@] in@ %a@]"
            pp_var_with_id x pp_var y (pp_comm 0) s)
    | IfNull (x, s, t) ->
      Format.fprintf ff "@[@[<2>if %a = null then@ %a@]@ @[<2>else@ %a@]@]"
        pp_var x (pp_comm 2) s (pp_comm 2) t
    | FunCall (x, ys) ->
      Format.fprintf ff "@[%a (@[%a@])@]" pp_var x (pp_list "," pp_var) ys
    | AssertEqVar (x, y) ->
      Format.fprintf ff "@[<2>assert@ (@[<2>%a =@ %a@])@]" pp_var x pp_var y
    | AssertEqDeref (x, y) ->
      Format.fprintf ff "@[<2>assert@ (@[<2>%a =@ *%a@])@]" pp_var x pp_var y
    | LetNewlock (x, s) ->
      pp_paren (p > 0) ff
        (fun ff ->
          Format.fprintf ff "@[<v>@[<2>let %a =@ newlock ()@] in@ %a@]"
            pp_var_with_id x (pp_comm 0) s)
    | Freelock x ->
      Format.fprintf ff "@[freelock %a@]" pp_var x
    | Acquire x ->
      Format.fprintf ff "@[acq %a@]" pp_var x
    | Release x ->
      Format.fprintf ff "@[rel %a@]" pp_var x
    | LetFork (x, s, t) ->
      pp_paren (p > 0) ff
        (fun ff ->
          Format.fprintf ff "@[<v>@[<2>let %a =@ @[<2>fork (@[<hv>%a@])@]@] in@ %a@]"
            pp_var_with_id x (pp_comm 0) s (pp_comm 0) t)
    | Wait x ->
      Format.fprintf ff "@[wait %a@]" pp_var x
  in
  pp_comm 0 ff

let pp_defn kwd ff (f, (ps, s)) =
  Format.fprintf ff "@[<v2>%s %a (@[<hv>%a@]) =@ %a@]"
    kwd pp_var_with_id f (pp_list ","  pp_var_with_id) ps pp_comm s

let rec pp_defns ff = function
  | [] -> ()
  | d :: ds ->
    Format.fprintf ff "%a@ %a"
      (pp_defn "and") d pp_defns ds

let pp_prog ff (ds, s) =
  match ds with
    | [] -> Format.fprintf ff "%a" pp_comm s
    | d :: ds ->
      Format.fprintf ff "@[<v>%a@ %a@]@,;;@ %a"
        (pp_defn "let rec") d pp_defns ds pp_comm s
  
let pp_print_comm ff = Format.fprintf ff "%a@." pp_comm
let print_comm = pp_print_comm Format.std_formatter
let pp_print_prog ff = Format.fprintf ff "%a@." pp_prog
let print_prog = pp_print_prog Format.std_formatter

(***** Typing *)
(* open Typing *)

(* let pp_ty () = *)
(*   let vars = ref [] in *)
(*   let var a = *)
(*     try List.assoc a !vars with *)
(*       | Not_found -> *)
(*         let i = List.length !vars in *)
(*         let s = String.make 1 (Char.chr ((Char.code 'a') + i mod 26)) ^ *)
(*           if i < 26 then "" else string_of_int (i / 26) *)
(*         in *)
(*         vars := (a, s) :: !vars; s *)
(*   in *)
(*   let rec pp_owner ff = function *)
(*     | VarO v -> Format.fprintf ff "?%d" v *)
(*     | FracO f -> Format.pp_print_float ff f *)
(*     | SumO _ -> assert false *)
(*   and pp_env ff = function *)
(*     | VarE v -> Format.fprintf ff "?%d" v *)
(*     | EnvE e -> *)
(*       let rec pp ff = function *)
(*         | [] -> () *)
(*         | [x, ty] -> *)
(*           Format.fprintf ff "%s: %a" x pp_ty ty *)
(*         | (x, ty) :: e -> *)
(*           Format.fprintf ff "%s: %a,@ %a" x pp_ty ty pp e *)
(*       in *)
(*       Format.fprintf ff "{@[<hv>%a@]}" pp e *)
(*   and pp_ty ff = function *)
(*     | VarT a -> *)
(*       Format.fprintf ff "'%s" (var a) *)
(*     | RefT (o, t) -> *)
(*       Format.fprintf ff "@[(@[<hv>%a,@ %a@]) ref@]" *)
(*         pp_ty t pp_owner o *)
(*     | RecRefT o -> *)
(*       let v = fresh_tvar () in *)
(*       Format.fprintf ff "@[<2>(@[<hv>'%s,@ %a@]) ref as '%s@]" *)
(*         (var v) pp_owner o (var v) *)
(*     | PidT (o, e) -> *)
(*       Format.fprintf ff "@[(@[<hv>%a,@ %a@]) pid@]" *)
(*         pp_env e pp_owner o *)
(*     | LockT (o, l, e) -> *)
(*       Format.fprintf ff "@[(@[<hv>%a,@ %a,@ %a@]) lock@]" *)
(*         pp_env e pp_owner l pp_owner o *)
(*   in *)
(*   pp_ty *)
