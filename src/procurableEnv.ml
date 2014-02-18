open Syntax

exception Error of string
let error s = raise (Error s)

type ct =
  | UprC of varid * (varid list)
  | LwrC of varid * (varid list)
  | EqC of varid * varid
  | EqSubstC of varid * (varid * varid) list * varid

let string_of_ct = function
  | UprC (x, yy) -> string_of_int x ^ " <= " ^ "[" ^ String.concat "; " (List.map string_of_int yy) ^ "]"
  | LwrC (x, yy) -> "[" ^ String.concat "; " (List.map string_of_int yy) ^ "]" ^ " <= " ^ string_of_int x
  | EqC (x, y) -> string_of_int x ^ " == " ^ string_of_int y
  | EqSubstC (x, s, y) -> string_of_int x ^ " == " ^ "[" ^ String.concat "; " (List.map (fun (sx, sy) -> string_of_int sx ^ "/" ^ string_of_int sy) s) ^ "]" ^ string_of_int y

let rec gen_constraint_comm fenv env s =
  let cc =
    match s with
      | Skip -> []
      | Assign (x, y) ->
        [UprC (Env.identify x env, []);
         UprC (Env.identify y env, [])]
      | Sequence (s, t) ->
        gen_constraint_comm fenv env s @ gen_constraint_comm fenv env t
      | Free _ ->
        []
      | LetMalloc ((x, i), s) ->
        UprC (i, []) ::
          (let env = Env.extend (x, i) () env in
           gen_constraint_comm fenv env s)
      | LetNull ((x, i), s) ->
        UprC (i, []) ::
          (let env = Env.extend (x, i) () env in
           gen_constraint_comm fenv env s)
      | LetVar ((x, i), y, s) ->
        EqC (i, Env.identify y env) ::
          (let env = Env.extend (x, i) () env in
           gen_constraint_comm fenv env s)
      | LetDeref ((x, i), _, s) ->
        UprC (i, []) ::
          (let env = Env.extend (x, i) () env in
           gen_constraint_comm fenv env s)
      | IfNull (_, s, t) ->
        gen_constraint_comm fenv env s @ gen_constraint_comm fenv env t
      | FunCall (f, aa) ->
        let iip = Env.lookup f fenv in
        let iia = List.map (fun a -> Env.identify a env) aa in
        let iipa = List.combine iip iia in
        List.map
          (fun (ip, ia) -> EqSubstC (ip, iipa, ia))
          iipa
      | AssertEqVar (x, y) ->
        [EqC (Env.identify x env, Env.identify y env)]
      | AssertEqDeref _ -> []
      | LetNewlock ((x, i), s) ->
        UprC (i, Env.ids env) ::
          (let env = Env.extend (x, i) () env in
           gen_constraint_comm fenv env s)
      | Freelock _ -> []
      | Acquire _ -> []
      | Release _ -> []
      | LetFork ((x, i), s, t) ->
        let vv = freevars s in
        let ii = List.map (fun x -> Env.identify x env) vv in
        UprC (i, Env.ids env) ::
          LwrC (i, ii) ::
          gen_constraint_comm fenv env s @
          (let env = Env.extend (x, i) () env in
           gen_constraint_comm fenv env t)
      | Wait _ -> []
  in
  (* prerr_string (String.make 20 '-' ^ "\n"); *)
  (* pp_print_comm Format.err_formatter s; *)
  (* prerr_newline (); *)
  (* List.iter (fun c -> prerr_string (string_of_ct c ^ "\n")) cc; *)
  cc

let rec swap = function
  | [] -> []
  | (a, b) :: ss -> (b, a) :: (swap ss)

let subst ss x =
  (* prerr_string ("*** " ^ string_of_int x ^ " :: "); *)
  (* List.iter (fun (a, b) -> prerr_string (" " ^ string_of_int a ^ ":" ^ string_of_int b)) ss; *)
  (* prerr_newline (); *)
  let rec subst ss x =
    match ss with
      | [] -> x
      | (a, b) :: _ when x = a -> b
      | _ :: ss -> subst ss x
  in
  subst ss x

let resolve_constraint cc =
  (* prerr_string (String.make 20 '-' ^ "\n"); *)
  (* prerr_string "INPUT:\n"; *)
  (* let b2 = [0; 1] in *)
  (* let ss = [5, 0; 6, 1; 7, 2] in *)
  (* let ss' = swap ss in *)
  (* List.iter (fun (a, b) -> print_string (string_of_int a ^ "/" ^ string_of_int b ^ ";;")) ss' ; *)
  (* print_string (string_of_int (subst ss' 1)); *)
  (* let b2' = List.map (subst ss') b2 in *)
  (* List.iter (fun i -> print_string (string_of_int i ^ "-")) b2' ; *)

  (* List.iter (fun c -> prerr_string (string_of_ct c ^ "\n")) cc; *)
  let upr, lwr, cc =
    List.fold_left
      (fun (upr, lwr, cc) -> function
        | UprC (x, yy) ->
          let upr =
            try
              let ux = List.assoc x upr in
              ux := ListSet.inter !ux yy;
              upr
            with
              | Not_found -> (x, ref yy) :: upr
          in
          upr, lwr, cc
        | LwrC (x, yy) ->
          let lwr =
            try
              let lx = List.assoc x lwr in
              lx := ListSet.union !lx yy;
              lwr
            with
              | Not_found -> (x, ref yy) :: lwr
          in
          upr, lwr, cc
        | (EqC _ | EqSubstC _) as c ->
          upr, lwr, c :: cc)
      ([], [], [])
      cc
  in

  (* prerr_string (String.make 20 '-' ^ "\n"); *)
  (* prerr_string "FOLDED:\n"; *)
  (* List.iter (fun c -> prerr_string (string_of_ct c ^ "\n")) cc; *)
  
  (* print_string (String.make 20 '>'); print_newline (); *)
  (* List.iter (fun (x, yy) -> *)
  (*   Printf.printf "%s = [%s]\n" (string_of_int x) (String.concat "; " (List.map string_of_int yy))) *)
  (*   upr; *)
  (* print_string (String.make 20 '-'); print_newline (); *)
  (* List.iter (fun c -> *)
  (*   Printf.printf "%s\n" (string_of_ct c)) *)
  (*   cc; *)
  (* print_string (String.make 20 '<'); print_newline (); *)

  let flag = ref true in
  while !flag do
    flag := false;
    (* prerr_string "BOUND:\n"; *)
    (* List.iter (fun (x, { contents = yy }) -> prerr_string (string_of_int x ^ ": " ^ (String.concat ", " (List.map string_of_int yy)) ^ "\n")) upr; *)
    List.iter
      (function
        | UprC _ | LwrC _ -> assert false
        | EqC (x, y) when x <> y ->
          let ux = List.assoc x upr
          and uy = List.assoc y upr in
          let ux' = ListSet.inter !ux !uy in
          if List.length !ux <> List.length ux' then
            (ux := ux'; flag := true)
        | EqC _ -> ()
        | EqSubstC (x, s, y) ->
          let ux = List.assoc x upr
          and uy = List.assoc y upr in
          let ux' = ListSet.inter !ux (List.map (subst (swap s)) !uy)
          and uy' = ListSet.inter !uy (List.map (subst s) !ux) in
          (* prerr_string ("* " ^ string_of_ct c ^ "\n"); *)
          (* prerr_string ("*- ux: " ^ string_of_int x ^ ": " ^ (String.concat ", " (List.map string_of_int !ux)) ^ "\n"); *)
          (* prerr_string ("*- uy: " ^ string_of_int y ^ ": " ^ (String.concat ", " (List.map string_of_int !uy)) ^ "\n"); *)
          (* prerr_string ("*- ux-: " ^ string_of_int x ^ ": " ^ (String.concat ", " (List.map string_of_int (List.map (subst (swap s)) !uy))) ^ "\n"); *)
          (* prerr_string ("*- uy-: " ^ string_of_int y ^ ": " ^ (String.concat ", " (List.map string_of_int (List.map (subst s) !ux))) ^ "\n"); *)
          (* prerr_string ("*- ux': " ^ string_of_int x ^ ": " ^ (String.concat ", " (List.map string_of_int ux')) ^ "\n"); *)
          (* prerr_string ("*- uy': " ^ string_of_int y ^ ": " ^ (String.concat ", " (List.map string_of_int uy')) ^ "\n"); *)
          if List.length !ux <> List.length ux' then
            (ux := ux'; flag := true);
          if List.length !uy <> List.length uy' then
            (uy := uy'; flag := true);      
      )
      cc
  done;
  (* prerr_string "BOUND:\n"; *)
  (* List.iter (fun (x, { contents = yy }) -> *)
  (*   prerr_string (string_of_int x ^ ": " ^ (String.concat ", " (List.map string_of_int yy)) ^ "\n")) upr; *)
  List.iter (fun (x, { contents = lx }) ->
    let ux = !(List.assoc x upr) in
    if not (ListSet.subset lx ux) then
      error "PTE constraint unsatisfiable") lwr;
  List.map (fun (x, u) -> x, !u) upr

let rec take n =
  if n = 0 then
    fun _ -> []
  else
    function
      | [] -> assert false
      | x :: l -> x :: (take (n - 1) l)

let type_prog (dd, s) =
  let fenv, cc = List.fold_left
    (fun (fenv, cc) (f, (pp, _)) ->
      let iip = (List.map snd pp) in
      Env.extend f iip fenv,
      cc @ (snd (List.fold_left (fun (i, c) ip -> i + 1, UprC (ip, take i iip) :: c) (0, []) iip)))
    (Env.empty, []) dd
  in
  let cc = List.fold_left
    (fun cc (_, (pp, s)) -> 
      let env = List.fold_left
        (fun env (x, i) -> Env.extend (x, i) () env)
        Env.empty pp in
      let cc' = gen_constraint_comm fenv env s in
      cc' @ cc)
    cc dd
  in
  let cc =
    let cc' = gen_constraint_comm fenv Env.empty s in
    cc' @ cc
  in
  cc

