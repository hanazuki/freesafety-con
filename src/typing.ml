open Syntax

type ovar = int
let fresh_ovar =
  let n = ref 0 in
  fun () ->
    let i = !n in
    incr n; i

type owner =
  | VarO of ovar
  | LitO of int
  | SumO of owner * owner

let zeroO = LitO 0
and oneO = LitO 1

let varO () = VarO (fresh_ovar ())

let rec string_of_owner = function
  | VarO v -> "f_" ^ (string_of_int v)
  | LitO i -> string_of_int i ^ "."
  | SumO (o1, o2) -> "(+ " ^ string_of_owner o1 ^ " " ^ string_of_owner o2 ^ ")"

let (+%) o1 o2 = SumO (o1, o2)

type ty =
  | AnyT
  | RefT of owner * owner
  | LockT of penv * owner * owner
  | PidT of penv * owner
and penv = (varid * ty) list

type fty = (varid * (ty * ty)) list

let rec string_of_ty = function
  | AnyT -> "any"
  | RefT (o1, o2) -> "(\\mu a. ref[" ^ string_of_owner o1 ^ "] a) ref[" ^ string_of_owner o2 ^ "]"
  | LockT (pe, o1, o2) -> string_of_penv pe ^ " lock[" ^ string_of_owner o1 ^ ", " ^ string_of_owner o2 ^ "]"
  | PidT (pe, o1) -> string_of_penv pe ^ " tid[" ^ string_of_owner o1 ^ "]"
and string_of_penv pe =
  "{" ^ (String.concat ", " (List.map (fun (i, t) -> string_of_int i ^ ": " ^ string_of_ty t) pe)) ^ "}"

let pptype i t =
  prerr_string (string_of_int i ^ ": " ^ string_of_ty t);
  prerr_newline ()

let (++%) tx ty =
  match tx, ty with
    | AnyT, AnyT -> AnyT
    | RefT (ox1, ox2), RefT (oy1, oy2) ->
      RefT (ox1 +% oy1, ox2 +% oy2)
    | LockT (pex, ox1, ox2), LockT (_pey, oy1, oy2) (*when pex = pey*) ->
      LockT (pex, ox1 +% oy1, ox2 +% oy2)
    | PidT (pex, ox), PidT (_pey, oy) (*when pex = pey*) ->
      PidT (pex, ox +% oy)
    | _ ->
      prerr_string "***";
      pptype (-1) tx;
      pptype (-2) ty;
      assert false


let freshen = function
  | AnyT -> AnyT
  | RefT (_, _) -> RefT (varO (), varO ())
  | LockT (pe, _, _) -> LockT (pe, varO (), varO ())
  | PidT (pe, _) -> PidT (pe, varO ())

let deref = function
  | RefT (o1, _) -> RefT (o1, o1)
  | _ -> assert false

type owner_ineq =
  | EqlOI of owner * owner
  | PosOI of owner

let string_of_ineq = function
  | EqlOI (o1, o2) -> "(= " ^ string_of_owner o1 ^ " "^ string_of_owner o2 ^ ")"
  | PosOI o -> "(< 0." ^ " " ^ string_of_owner o ^ ")"

let (=%) o1 o2 = EqlOI (o1, o2)
let pos o = PosOI o
let empty = function
  | AnyT -> []
  | RefT (o1, o2) -> [o1 =% zeroO; o2 =% zeroO]
  | LockT (_, o1, o2) -> [o1 =% zeroO; o2 =% zeroO]
  | PidT (_, o1) -> [o1 =% zeroO]

let (==%) tx ty =
  match tx, ty with
    | AnyT, AnyT -> []
    | RefT (ox1, ox2), RefT (oy1, oy2) ->
      [ox1 =% oy1; ox2 =% oy2]
    | LockT (pex, ox1, ox2), LockT (pey, oy1, oy2) when pex = pey ->
      [ox1 =% oy1; ox2 =% oy2]
    | PidT (pex, ox), PidT (pey, oy) when pex = pey ->
      [ox =% oy]
    | _ -> assert false

let readable = function
  | RefT (_, o2) -> [pos o2]
  | _ -> assert false

let list_union aa bb =
  List.fold_left (fun aa b -> if (List.mem b aa) then aa else (b::aa)) aa bb

let (===%) pex pey =
  let ii = list_union (fst (List.split pex)) (fst (List.split pey)) in
  List.fold_left (fun c i -> (List.assoc i pex ==% List.assoc i pey) @ c) [] ii

let rec translate st pe i =
  let sti = List.assoc i st in
  let pei = List.assoc i pe in
  match sti with
    | SimpleType.VarST _ -> AnyT
    | SimpleType.RefST -> RefT (VarO (fresh_ovar ()), VarO (fresh_ovar ()))
    | SimpleType.LockST ->
      let pp = List.map (fun i -> i, translate st pe i) pei in
      LockT (pp, VarO (fresh_ovar ()), VarO (fresh_ovar ()))
    | SimpleType.PidST ->
      let pp = List.map (fun i -> i, translate st pe i) pei in
      PidT (pp, VarO (fresh_ovar ()))

let preprocess prog =
  let st = SimpleType.type_prog prog
  and pe = ProcurableEnv.resolve_constraint (ProcurableEnv.type_prog prog) in
  st, pe

let rec conv vap = function
  | AnyT -> AnyT
  | RefT _ as ty -> ty
  | LockT (pe, o1, o2) -> LockT (convpe vap pe, o1, o2)
  | PidT (pe, o) -> PidT (convpe vap pe, o)
and convpe vap =
  List.map
    (fun (i, ty) ->
      try
        let i' = List.assoc i vap in
        (i', ty)
      with Not_found ->
        (i, ty))

let rec gen_ineq_comm fenv env vmap s =
  let c, e = match s with
    | Skip -> [], env
    | Assign (x, y) ->
      let tx = Env.lookup x env
      and ty = Env.lookup y env in
      let tx' = RefT (varO (), oneO)
      and ty' = freshen ty in
      (tx ==% RefT (zeroO, oneO)) @ (ty ==% deref tx' ++% ty'),
      Env.update y ty (Env.update x tx env)
    | Sequence (s, t) ->
      let cs, env = gen_ineq_comm fenv env vmap s in
      let ct, env = gen_ineq_comm fenv env vmap t in
      cs @ ct, env
    | Free x ->
      let tx = Env.lookup x env in
      tx ==% RefT (zeroO, oneO), Env.update x (RefT (zeroO, zeroO)) env
    | LetMalloc ((x, i), s) ->
      let tx = RefT (zeroO, oneO) in
      pptype i tx;
      let env = Env.extend (x, i) tx env in
      let cs, env = gen_ineq_comm fenv env vmap s in
      empty (Env.lookup x env) @ cs, Env.remove x env
    | LetNull ((x, i), s) ->
      let tx = RefT (varO (), varO ()) in
      pptype i tx;
      let env = Env.extend (x, i) tx env in
      let cs, env = gen_ineq_comm fenv env vmap s in
      empty (Env.lookup x env) @ cs, Env.remove x env
    | LetVar ((x, i), y, s) ->
      let ty = Env.lookup y env in
      let tx = freshen ty and ty' = freshen ty in
      pptype i tx;
      let env = Env.extend (x, i) tx (Env.update y ty' env) in
      let cs, env = gen_ineq_comm fenv env vmap s in
      (ty ==% tx ++% ty') @ empty (Env.lookup x env) @ cs, Env.remove x env
    | LetDeref ((x, i), y, s) ->
      let ty = Env.lookup y env in
      let tx = freshen (deref ty) in
      pptype i tx;
      let ty' = match ty with RefT (_, o2) -> RefT (varO (), o2) | _ -> assert false in
      let env = Env.extend (x, i) tx (Env.update y ty' env) in
      let cs, env = gen_ineq_comm fenv env vmap s in
      readable ty @ (deref ty ==% tx ++% deref ty') @ empty (Env.lookup x env) @ cs, Env.remove x env
    | IfNull (x, s, t) ->
      let tx = Env.lookup x env in
      let cs, envs = gen_ineq_comm fenv (Env.update x (freshen tx) env) vmap s
      and ct, envt = gen_ineq_comm fenv env vmap t in
      (Env.unname envs ===% Env.unname envt) @ cs @ ct, envt
    | FunCall (f, aa) ->
      let fty = Env.lookup f fenv in
      let vap = List.combine (fst (List.split fty)) (List.map (fun a -> Env.identify a env) aa) in
      List.fold_left
        (fun (cs, env') ((_, (tp, tp')), a) ->
          let ta = Env.lookup a env in
          let tt = freshen ta in
          (ta ==% tt ++% conv vap tp) @ cs, Env.update a (tt ++% conv vap tp') env')
        ([], env) (List.combine fty aa)
    | AssertEqVar (x, y) ->
      let tx = Env.lookup x env
      and ty = Env.lookup y env in
      let tx' = freshen tx
      and ty' = freshen ty in
      tx ++% ty ==% tx' ++% ty', Env.update x tx' (Env.update y ty' env)
    | AssertEqDeref (x, y) ->
      let tx = Env.lookup x env
      and ty = Env.lookup y env in
      let tx' = freshen tx
      and ty' = match ty with RefT (_, o2) -> RefT (varO (), o2) | _ -> assert false in
      readable ty @ (tx ++% deref ty ==% tx' ++% deref ty'), Env.update x tx' (Env.update y ty' env)
    | LetNewlock ((x, i), s) ->
      let cv = List.assoc i vmap in
      let c, pe, env = List.fold_left
        (fun (c, pe, env) j ->
          let tj = Env.lookup_by_id j env in
          let tj' = freshen tj and tj'' = freshen tj in
          (tj ==% tj' ++% tj'') @ c, (j, tj'') :: pe, Env.update_by_id j tj' env)
        ([], [], env) cv
      in
      let tx = LockT (pe, zeroO, oneO) in
      pptype i tx;
      let cs, env = gen_ineq_comm fenv (Env.extend (x, i) tx env) vmap s in
      empty (Env.lookup x env) @ cs @ c, Env.remove x env
    | Freelock x ->
      let tx = Env.lookup x env in
      (match tx with
        | LockT (pe, o1, o2) ->
          [o1 =% zeroO; o2 =% oneO],
          List.fold_left
            (fun env (i, t) -> Env.update_by_id i (Env.lookup_by_id i env ++% t) env)
            (Env.update x (LockT (pe, zeroO, zeroO)) env) pe
        | _ -> assert false)
    | Acquire x ->
      let tx = Env.lookup x env in
      (match tx with
        | LockT (pe, o1, o2) ->
          let c, env = List.fold_left
            (fun (c, env) (i, ti) ->
              let t = Env.lookup_by_id i env in
              let t' = freshen t in
              (t' ==% t ++% ti) @ c, Env.update_by_id i t' env)
            ([], env) pe
          in
          [o1 =% zeroO; pos o2] @ c, Env.update x (LockT (pe, oneO, o2)) env
        | _ -> assert false)
    | Release x ->
      let tx = Env.lookup x env in
      (match tx with
        | LockT (pe, o1, o2) ->
          let c, env = List.fold_left
            (fun (c, env) (i, ti) ->
              let t = Env.lookup_by_id i env in
              let t' = freshen t in
              (t ==% t' ++% ti) @ c, Env.update_by_id i t' env)
            ([], env) pe
          in
          [o1 =% oneO; pos o2] @ c, Env.update x (LockT (pe, zeroO, o2)) env
        | _ -> assert false)
    | LetFork ((x, i), s, t) ->
      let cv = List.assoc i vmap in
      let c, env', env'' = List.fold_left
        (fun (c, env', env'') j ->
          let tj = Env.lookup_by_id j env in
          let tj' = freshen tj and tj'' = freshen tj in
          (tj ==% tj' ++% tj'') @ c, Env.update_by_id j tj' env', Env.update_by_id j tj'' env'')
        ([], env, env) cv
      in
      let cs, env' = gen_ineq_comm fenv env' vmap s in
      let pe = List.map (fun j -> j, Env.lookup_by_id j env') cv in
      let tx = PidT (pe, oneO) in
      pptype i tx;
      let env'' = Env.extend (x, i) tx env'' in
      let ct, env'' = gen_ineq_comm fenv env'' vmap t in
      empty (Env.lookup x env'') @ cs @ ct @ c, Env.remove x env''
    | Wait x ->
      let tx = Env.lookup x env in
      (match tx with
        | PidT (pe, o) ->
          [o =% oneO],
          List.fold_left
            (fun env (i, t) -> Env.update_by_id i (Env.lookup_by_id i env ++% t) env)
            (Env.update x (PidT (pe, zeroO)) env) pe
        | _ -> assert false)
  in
  (* prerr_string (String.make 20 '-' ^ "\n"); *)
  (* Env.iter (fun _ i a -> pptype i a) env; *)
  (* pp_print_comm Format.err_formatter s; *)
  (* Env.iter (fun _ i a -> pptype i a) e; *)
  (* prerr_newline (); *)
  (* List.iter (fun c -> prerr_string (string_of_ineq c)) c; *)
  (* prerr_newline (); *)
  (c, e)

let gen_ineq_defn fenv vmap ((_, i), (pp, s)) =
  let tf = Env.lookup_by_id i fenv in
  let tta = List.map (fun (i, (t, _)) -> i, t) tf in
  let env = List.fold_left
    (fun env (x, i) -> Env.extend (x, i) (List.assoc i tta) env)
    Env.empty pp
  in
  gen_ineq_comm fenv env vmap s

let rec vars_owner = function
  | VarO i -> [i]
  | LitO _ -> []
  | SumO (o1, o2) -> list_union (vars_owner o1) (vars_owner o2)

let vars_ineq = function
  | EqlOI (o1, o2) -> list_union (vars_owner o1) (vars_owner o2)
  | PosOI o -> vars_owner o

let gen_ineq_prog (dd, s as prog) =
  let st, pe = preprocess prog in
  let fenv = List.fold_left
    (fun env (f, (pp, _)) ->
      let (tf: fty) = List.map
        (fun (_, j) ->
          let tp = translate st pe j in
          j, (tp, freshen tp))
        pp
      in
      Env.extend f tf env)
    Env.empty dd
  in
  let cf = List.fold_left
    (fun cf ((_, i), _ as d) ->
      let c, env = gen_ineq_defn fenv pe d in
      let tf = Env.lookup_by_id i fenv in
      let ttp = List.map (fun (i, (_, t)) -> i, t) tf in
      c @ List.flatten (List.map (fun (i, t) -> t ==% Env.lookup_by_id i env) ttp) @ cf)
    [] dd
  in
  let cs, env = (gen_ineq_comm fenv Env.empty pe s) in
  let cs' = Env.fold (fun _ t cs' -> empty t @ cs') [] env in
  let c = cf @ cs @ cs' in
  let vv = List.fold_left list_union [] (List.map vars_ineq c) in
  vv, c
