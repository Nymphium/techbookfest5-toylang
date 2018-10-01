module T = Term
open Irepr

module SS = Set.Make(struct
    type t = string
    let compare = (compare: t -> t -> int)
  end)

let rec get_fv = T.(function
    | Add(e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2)
    | Eq(e1, e2) | Lt(e1, e2) | Ge(e1, e2) | App(e1, e2) -> SS.union (get_fv e1) (get_fv e2)
    | If(c, e1, e2) -> SS.(union (get_fv c) @@ union (get_fv e1) (get_fv e2))
    | Not e -> get_fv e
    | Int _ | Bool _ | Unit -> SS.empty
    | Var x -> SS.singleton x
    | Fun(x, e) -> SS.remove x @@ get_fv e
    | Let(x, e, body) -> SS.(union (get_fv e) @@ remove x @@ get_fv body) 
    | LetRecFun(f, x, body, e) -> SS.(union (get_fv body |> (remove x)) (get_fv e) |> (remove f)))

let var_to_reg xs (env: (string * int) list) = xs |> List.map @@ fun x -> List.assoc x env

let squash ls =
  let rec work acc = function
    | [] -> acc
    | ("_", _) :: kvs -> work acc kvs
    | ((x, _) as kv) :: kvs ->
      if List.exists (fun (y, _) -> x = y) acc then
        work acc kvs
      else
        work (kv :: acc) kvs
  in work [] ls

let[@inline] get_last = function
  | Return(reg) :: is -> reg, is
  | TailCall(reg, arg) :: is -> reg, Call(reg, arg) :: is
  | _ -> invalid_arg "get_last"

let[@inline]compile0 term =
  let rec work term fvs env =
    let create_ref () =
      let r = ref (-1) in
      r, fun () -> incr r; !r
    in
    let regref, regi = create_ref () in
    ignore @@ regi ();
    let constref, consti = create_ref () in
    let closref, closi = create_ref () in
    let rec inner term is cs cls fvs env =
      match term with
      | T.Unit ->
        let reg = regi () in
        (Return(reg) :: Unit(reg) :: is), cs, cls, fvs, env
      | T.Bool b ->
        let reg = regi () in
        (Return(reg) :: SetBool(reg, (if b then 1 else 0), 0) :: is), cs, cls, fvs, env
      | T.Int i ->
        let reg = regi () in
        let rint = RInt i in
        let cs', ci =
          match Util.find_opt_with_index ((=) rint) cs with
          | Some(_, idx) -> cs, idx
          | None -> cs @ [rint], consti ()
        in
        let is' = (Return(reg) :: Load(reg, ci) :: is) in
        is', cs', cls, fvs, env
      | T.Var x ->
        let reg = regi () in
        let idx = List.assoc x env in
        let ist =
          if idx >= 0 then
            Move(reg, idx)
          else
            Upval(reg, (-idx - 1))
        in
        Return(reg) :: ist :: is, cs, cls, fvs, env
      | T.Let(x, e, body) ->
        let is', cs', cls', fvs', env' = inner e is cs cls fvs env in
        let latest_reg, is' = get_last is' in
        let env'' = (x, latest_reg) :: env' in
        inner body (Move(regi(), latest_reg) :: is') cs' cls' fvs' env'' 
      | T.If(c, e1, e2) ->
        let is', cs', cls', fvs', env' = inner c is cs cls fvs env in
        let rc, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e1 is' cs' cls' fvs' env' in
        let re1, is'' = get_last is'' in
        let is''', cs''', cls''', fvs''', env''' = inner e2 is'' cs'' cls'' fvs'' env'' in
        let re2, is''' = get_last is''' in
        let reg = !regref in
        let isl, isl1, isl2 = List.(length is', length is'', length is''') in
        (Return(reg) :: Move(reg, re2) :: (Util.take (isl2 - isl1) is''')
         @ Jump(isl2 - isl1 + 1) :: Move(reg, re1) :: (Util.take (isl1 - isl) is'')
         @ Jump(isl1 - isl + 2) :: Test(rc, 0) :: is'), cs''', cls''', fvs''', env'''
      | T.Not(e) ->
        let is', cs', cls', fvs', env' = inner e is cs cls fvs env in
        let re1, is' = get_last is' in
        let reg = regi () in
        (Return(reg) :: SetBool(reg, 0, 0) :: SetBool(reg, 1, 1) :: Test(re1, 0) :: is'), cs', cls', fvs', env'
      | T.Eq(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: SetBool(reg, 0, 0) :: SetBool(reg, 1, 1) :: Eq(re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Lt(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: SetBool(reg, 0, 0) :: SetBool(reg, 1, 1) :: Lt(re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Ge(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: SetBool(reg, 0, 0) :: SetBool(reg, 1, 1) :: Ge(re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Add(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: Add(reg, re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Sub(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: Sub(reg, re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Mul(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: Mul(reg, re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Div(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        let reg = regi () in
        (Return(reg) :: Div(reg, re1, re2) :: is''), cs'', cls'', fvs'', env''
      | T.Fun(x, e) ->
        let reg = regi () in
        let fv = get_fv term in
        (* create new environment with NEEDED var-reg pairs from the old *)
        let env0 = env |> List.filter @@ fun (x, _) -> SS.mem x fv in
        (* upvalues are indexed by -(i + 1) *)
        let env' = (squash env0 |> List.mapi (fun i (x, _) -> (x, -(i + 1)))) in
        (* fvs is map from Upval's index to current register:
         * create closure on the I with upvalues; Upval(_, i) refers fvs[i] -> CURRENT Reg[fvs[i]]
        *)
        let fvs' = var_to_reg (SS.fold (fun x z -> x :: z) fv []) env0 in
        let Q(isf, csf, clsf, _) = work e fvs' ((x, 0) :: env') in
        (* append fvs map; Upvalue store is growing *)
        let cl' = Q(isf, csf, clsf, fvs' @ fvs) in
        (Return(reg) :: Clos(reg, closi (), 0) :: is), cs, cls @ [cl'], fvs, env
      | T.LetRecFun(f, x, body, e) ->
        (* access the function itself in its call by upvalue *)
        let reg = regi () in
        let fv = get_fv term in
        let env0 = (f, reg) :: (env |> List.filter @@ fun (x, _) -> SS.mem x fv) in
        let env' = (squash env0 |> List.mapi (fun i (x, _) -> (x, -(i + 1)))) in
        let fvs' = var_to_reg (SS.fold (fun x z -> x :: z) fv []) env0 in
        let Q(isf, csf, clsf, _) = work body fvs' ((x, 0) :: env') in
        let cl' = Q(isf, csf, clsf, fvs @ fvs') in
        let is' = Clos(reg, closi (), 1) :: is in
        inner e is' cs (cl' :: cls) fvs @@ (f, reg) :: env
      | T.App(e1, e2) ->
        let is', cs', cls', fvs', env' = inner e1 is cs cls fvs env in
        let re1, is' = get_last is' in
        let is'', cs'', cls'', fvs'', env'' = inner e2 is' cs' cls' fvs' env' in
        let re2, is'' = get_last is'' in
        (TailCall(re1, re2) :: is''), cs'', cls'', fvs'', env''
    in
    let is, cs, cls, fvs, env' = inner term [] [] [] [] env in
    List.(Q(rev is, cs, cls, fvs))
  in work term [] []

let compile t = compile0 t

