type i = (* 'r is type of register *)
  | Add of int * int * int (* Add(a, b, c) = Reg[a] = Reg[b] + Reg[c] *)
  | Sub of int * int * int
  | Mul of int * int * int
  | Div of int * int * int
  | Unit of int (* Unit(a) = Reg[a] = Unit *)
  | SetBool of int * int * int (* SetBool(a, x, p) = Reg[a] = x != 0; if p = 1 then pc++ *)
  | Eq of int * int (* Eq(a, b) =  if Reg[a] == Reg[b] then pc++ *)
  | Lt of int * int (* Lt(a, b) =  if Reg[a] > Reg[b] then pc++ *)
  | Ge of int * int (* Ge(a, b) =  if Reg[a] <= Reg[b] then pc++ *)
  | Test of int * int (* Test(a, b) = if (b > 0 && Reg[a]) || (b <= 0 && not Reg[a]) then pc++ *)
  | Jump of int (* Jump(a) = pc += a *)
  | Load of int * int (* const load; Load(a, b) = Reg[a] = Const[b] *)
  | Clos of int * int * int (* closure load; Clos(a, b, p) = Reg[a] = Clos[b] with recursiveness(p = 0 ?) *)
  | Call of int * int (* Call(a, b) = Reg[a] = Reg[a](b)  *)
  | Move of int * int (* Move(a, b) = Reg[a] = b *)
  | Upval of int * int (* Upval(a, b) = Reg[a] = Upval[b] *)
  | Return of int (* Return(i) = exit with Reg[i] *)
  | TailCall of int * int (* exit with Reg[a](b) *)
(* VM values *)
and regv =
  | Null (* initialized value *)
  | Vunit
  | Vint of int
  | Vbool of bool
  | VC of cl * regv list ref
and reg = regv ref array
and rawval =
  | RInt of int
  (* values *)
and cs = rawval list
(* closure *)
and cl = Q of i list * cs * cl list * int list
[@@deriving show { with_path = false }]

