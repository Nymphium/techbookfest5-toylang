open Irepr
open VMUtils

let run (Q(is, cs, cls, _)) =
  let rec work is cs cls upvs (rv : reg) =
    match is with
    | [] -> raise @@ Failure "exit without RETURN"
    | Return(i) :: _ -> !(rv.(i))
    | Jump(i) :: is' -> work (Util.drop i is') cs cls upvs rv
    | Test(a, p) :: is' ->
      let b = ?@ !(rv.(a)) in
      if p > 0 && b || p <= 0 && not b then
        work (List.tl is') cs cls upvs rv
      else
        work is' cs cls upvs rv
    | Load(ra, ci) :: is' ->
      let v = match List.nth cs ci with
        | RInt i -> Vint i
      in
      let () = rv.(ra) := v in
      work is' cs cls upvs rv
    | Unit(a) :: is' ->
      let () = rv.(a) := Vunit in
      work is' cs cls upvs rv
    | SetBool(a, x, p) :: is' ->
      let () = rv.(a) := Vbool(x > 0) in
      let is'' =
        if p = 1 then List.tl is'
        else is'
      in
      work is'' cs cls upvs rv
    | Add(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) +@ !(rv.(c)) in
      work is' cs cls upvs rv
    | Sub(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) -@ !(rv.(c)) in
      work is' cs cls upvs rv
    | Mul(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) *@ !(rv.(c)) in
      work is' cs cls upvs rv
    | Div(a, b, c) :: is' ->
      let () = rv.(a) := !(rv.(b)) /@ !(rv.(c)) in
      work is' cs cls upvs rv
    | Lt(a, b) :: is' ->
      if ?@ (!(rv.(a)) <@ !(rv.(b))) then
        work (List.tl is') cs cls upvs rv
      else
        work is' cs cls upvs rv
    | Eq(a, b) :: is' ->
      if !(rv.(a)) = !(rv.(b)) then
        work (List.tl is') cs cls upvs rv
      else
        work is' cs cls upvs rv
    | Ge(a, b) :: is' ->
      if ?@ (!(rv.(a)) >=@ !(rv.(b))) then
        work (List.tl is') cs cls upvs rv
      else
        work is' cs cls upvs rv
    | Clos(a, b, p) :: is' ->
      let Q(isc, csc, clsc, fvsc) as c' = List.nth cls b in
      let upvs' = fvsc |> List.map (fun ri -> !(rv.(ri))) in
      let upr = ref upvs' in
      let cl = VC(c', upr) in
      let () = rv.(a) := cl in
      let () = if p = 1 then upr := cl :: !upr in
      work is' cs cls upvs rv
    | Move(a, b) :: is' ->
      let () = rv.(a) := !(rv.(b)) in
      work is' cs cls upvs rv
    | Upval(a, b) :: is' ->
      let () = rv.(a) := List.nth upvs b in
      work is' cs cls upvs rv
    | Call(a, b) :: is' ->
      let isf, csf, clsf, _, upr = unwrap_vclos rv a in
      let rv' = new_reg () in
      let () = rv'.(0) := !(rv.(b)) in
      let () = rv.(a) := work isf csf clsf (!upr @ upvs) rv' in
      work is' cs cls upvs rv
    | TailCall(a, b) :: is' ->
      let isf, csf, clsf, _, upr = unwrap_vclos rv a in
      let rv' = new_reg () in
      let () = rv'.(0) := !(rv.(b)) in
      work isf csf clsf (!upr @ upvs) rv'

  in
  work is cs cls [] @@ new_reg ()

