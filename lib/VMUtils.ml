open Irepr

let __REG_SIZE__ = 256

let[@inline] new_reg () = Array.init __REG_SIZE__ (fun _ -> ref Null)

let[@inline] (+@) (Vint i) (Vint j) = Vint(i + j)
let[@inline] (-@) (Vint i) (Vint j) = Vint(i - j)
let[@inline] ( *@) (Vint i) (Vint j) = Vint(i * j)
let[@inline] (/@) (Vint i) (Vint j) = Vint(i / j)
let[@inline] (<@) (Vint i) (Vint j) = Vbool(i < j)
let[@inline] (>=@) (Vint i) (Vint j) = Vbool(i >= j)

let[@inline] (?@) (Vbool b) = b

let[@inline] unwrap_vclos rv a =
  match !(rv.(a)) with
  | VC(Q(isf, csf, clsf, fvsf), upr) -> isf, csf, clsf, fvsf, upr
  | _ -> failwith "failed to unwrap"

