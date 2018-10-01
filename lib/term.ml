type t =
  | Bool of bool
  | Unit
  | Int of int
  | Fun of string * t
  | LetRecFun of string * string * t * t
  | Var of string
  | App of t * t
  | Let of string * t * t
  | If of t * t * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Not of t
  | Eq of t * t
  | Lt of t * t (* t < t *)
  | Ge of t * t (* t >= t *)
[@@deriving show { with_path = false }]


module DSL = struct
  let[@inline] (@) e1 e2 = App(e1, e2)

  let[@inline] add e1 e2 = Add(e1, e2)
  let[@inline] sub e1 e2 = Sub(e1, e2)
  let[@inline] mul e1 e2 = Mul(e1, e2)
  let[@inline] div e1 e2 = Div(e1, e2)

  let[@inline] lt e1 e2 = Lt(e1, e2)
  let[@inline] ge e1 e2 = Ge(e1, e2)
  let[@inline] eq e1 e2 = Eq(e1, e2)

  let[@inline] not e = Not(e)

  let[@inline] let_ x bde body = Let(x, bde, body)
  let[@inline] letrec f x bde body = LetRecFun(f, x, bde, body)
  let[@inline] fun_ x body = Fun(x, body)
  let[@inline] if_ c e1 e2 = If(c, e1, e2)

  let[@inline] (~&) i = Int i
  let[@inline] (~?) x = Var x
  let u = Unit
  let true_ = Bool true
  let false_ = Bool false
end

