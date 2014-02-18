open Syntax

type tvar = int
let fresh_tvar =
  let n = ref 0 in
  fun () -> incr n; !n

type sty =
  | VarST of tvar
  | RefST
  | LockST
  | PidST

let string_of_sty = function
  | VarST n -> "'_" ^ (string_of_int n)
  | RefST -> "ref"
  | LockST -> "lock"
  | PidST -> "pid"

module Subst : sig
  type t
  val return: (tvar * sty) -> t
  val apply: t -> sty -> sty
  val add: (tvar * sty) -> t -> t
  val concat: t -> t -> t
  val id: t
  val equations: t -> (sty * sty) list
end = struct
  type t = (tvar * sty) list
  
  let return s = [s] 
  
  let apply s = function
    | VarST tv as ty ->
      (try List.assoc tv s with Not_found -> ty)
    | (RefST | LockST | PidST) as ty -> ty

  let add (tv, ty) ss =
    let apply = function
      | VarST tv' as ty' -> if tv = tv' then ty else ty'
      | (RefST | LockST | PidST) as ty -> ty
    in
    (tv, ty) :: (List.map (fun (tv, ty) -> tv, apply ty) ss)
  
  let concat s s' =
    List.fold_right add s' s

  let id = []

  let equations ss =
    List.map (fun (tv, ty) -> VarST tv, ty) ss
end

let unify =
  let unify_aux s (tyl, tyr) =
    match Subst.apply s tyl, Subst.apply s tyr with
      | tyl, tyr when tyl = tyr -> s
      | VarST tv, ty | ty, VarST tv -> Subst.add (tv, ty) s
      | tyl, tyr -> failwith ("Cannot unify " ^ string_of_sty tyl ^ " with " ^ string_of_sty tyr)
  in
  List.fold_left unify_aux Subst.id

let rec gen_constraint_comm fenv env = function
  | Skip -> [], []
  | Assign (x, y) ->
    [], [Env.lookup x env, RefST;
         Env.lookup y env, RefST]
  | Sequence (s, t) ->
    let ms, ss = gen_constraint_comm fenv env s in
    let mt, st = gen_constraint_comm fenv env t in
    ms @ mt, ss @ st
  | Free x -> 
    [], [Env.lookup x env, RefST]
  | LetMalloc ((x, i), s) ->
    let tyx = RefST in
    let ms, ss =
      let env = Env.extend (x, i) tyx env in
      gen_constraint_comm fenv env s
    in
    (i, tyx) :: ms, ss
  | LetNull ((x, i), s) ->
    let tyx = RefST in
    let ms, ss =
      let env = Env.extend (x, i) tyx env in
      gen_constraint_comm fenv env s
    in
    (i, tyx) :: ms, ss
  | LetVar ((x, i), y, s) ->
    let tyx = Env.lookup y env in
    let ms, ss =
      let env = Env.extend (x, i) tyx env in
      gen_constraint_comm fenv env s
    in
    (i, tyx) :: ms, ss
  | LetDeref ((x, i), y, s) ->
    let tyx = RefST in
    let ms, ss =
      let env = Env.extend (x, i) tyx env in
      gen_constraint_comm fenv env s
    in
    (i, tyx) :: ms, (Env.lookup y env, RefST) :: ss
  | IfNull (x, s, t) ->
    let ms, ss = gen_constraint_comm fenv env s in
    let mt, st = gen_constraint_comm fenv env t in
    ms @ mt, (Env.lookup x env, RefST) :: ss @ st
  | FunCall (f, aa) ->
    [], (List.combine
           (List.map (fun a -> Env.lookup a env) aa)
           (Env.lookup f fenv))
  | AssertEqVar (x, y) ->
    let tv = fresh_tvar () in
    [], [Env.lookup x env, VarST tv;
         Env.lookup y env, VarST tv]
  | AssertEqDeref (x, y) ->
    [], [Env.lookup x env, RefST;
         Env.lookup y env, RefST]
  | LetNewlock ((x, i), s) ->
    let tyx = LockST in
    let ms, ss =
      let env = Env.extend (x, i) tyx env in
      gen_constraint_comm fenv env s
    in
    (i, tyx) :: ms, ss
  | Freelock x ->
    [], [Env.lookup x env, LockST]
  | Acquire x ->
    [], [Env.lookup x env, LockST]
  | Release x ->
    [], [Env.lookup x env, LockST]
  | LetFork ((x, i), s, t) ->
    let ms, ss = gen_constraint_comm fenv env s in
    let tyx = PidST in
    let mt, st =
      let env = Env.extend (x, i) tyx env in
      gen_constraint_comm fenv env t
    in
    (i, tyx) :: ms @ mt, ss @ st
  | Wait x ->
    [], [Env.lookup x env, PidST]

let type_prog (dd, s) =
  let vmap = List.concat
    (List.map
       (fun (_, (pp, _)) ->
         List.map (fun (_, i) -> i, VarST (fresh_tvar ())) pp)
       dd)
  in
  let fenv = List.fold_left
    (fun fenv (f, (pp, _)) ->
      Env.extend f (List.map (fun (_, i) -> List.assoc i vmap) pp) fenv)
    Env.empty dd
  in
  let (vmap, ss) = List.fold_left
    (fun (vmap, ss) (_, (pp, s)) -> 
      let env = List.fold_left
        (fun env (x, i) -> Env.extend (x, i) (List.assoc i vmap) env)
        Env.empty pp in
      let vmap', ss' = gen_constraint_comm fenv env s in
      vmap' @ vmap, ss' @ ss)
    (vmap, []) dd
  in
  let vmap, ss =
    let vmap', ss' = gen_constraint_comm fenv Env.empty s in
    vmap' @ vmap, ss' @ ss
  in
  List.map (fun (i, t) -> i, Subst.apply (unify ss) t) vmap
