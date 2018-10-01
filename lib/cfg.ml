open Irepr

type t0 = {
  from: int ref;
  _to: int ref;
  pred: s list ref;
  succ: s list ref;
  succ_pos: int list option ref
}
and s = T of t0

let[@inline] unwrap_option = function
  | Some(e) -> e
  | None -> failwith "unwrap failed"

let get_npos_block nth blocks =
  blocks |> List.find_opt @@ fun block ->
  !(block.from) <= nth && !(block._to) >= nth

let split_block block delimp =
  let newblock = {from = ref !(block.from); _to= ref (delimp - 1); pred= ref !(block.pred); succ= ref [T block]; succ_pos= ref @@ Some []} in
  let () =
    block.from := delimp;
    block.pred := [T newblock];
  in newblock

let insert blocks blk =
  if blk.from <= blk._to && not @@ List.exists (fun b -> b.from = blk.from && b._to = blk._to) blocks then
    List.sort (fun a b -> !(a.from) - !(b.from)) (blk :: blocks)
  else blocks

let insertT blocks blk =
  if blk.from <= blk._to && not @@ List.exists (fun (T b) -> b.from = blk.from && b._to = blk._to) blocks then
    List.sort (fun (T a) (T b) -> !(a.from) - !(b.from)) ((T blk) :: blocks)
  else blocks

let remove block idx =
  let _, block' = Util.extract_by_index !block idx in
  block := block'

let mkcfg0 is =
  let blocks : t0 list ref = ref [] in
  let () =
    is |> List.iteri @@ fun i op ->
    let succ_pos = ref @@ match op with
      | Jump(a) -> Some [a + i + 1]
      | SetBool(_, _, 1) -> Some [i + 1]
      | Eq(_, _) | Lt(_, _) | Ge(_, _) | Test(_, _) -> Some [i + 1; i + 2]
      | TailCall(_, _) | Return(_) -> Some []
      | _ -> None
    in
    blocks := {from = ref i; _to = ref i; pred = ref []; succ = ref []; succ_pos}
              :: !blocks
  in
  blocks := List.rev !blocks;
  let idx = ref 0 in
  let continue = ref true in
  let () = while !continue do
      match List.nth_opt !blocks !idx with
      | None -> continue := false
      | Some block ->
        if !(block.succ_pos) <> None then
          begin
            let succ_pos = unwrap_option @@ !(block.succ_pos) in
            let () = succ_pos |> List.iter @@ fun pos ->
              match get_npos_block pos !blocks with
              | Some blk_ ->
                let () = if !(blk_.from) < pos then
                    let newblk = split_block blk_ pos in
                    blocks := insert !blocks newblk
                in
                blk_.pred := insertT !(blk_.pred) block;
                block.succ := insertT !(block.succ) blk_;
              | None ->
                if List.length succ_pos > 0 then
                  block.succ_pos := Some(pos :: succ_pos);
            in
            block.succ_pos := None;
            blocks := Util.swap_by_index !idx !blocks block;

            incr idx;
          end
        else if List.length !(block.succ) = 0 then
          begin
            let nextblock = List.nth !blocks (!idx + 1) in
            if List.length !(nextblock.pred) > 0 then
              begin
                nextblock.pred := insertT !(nextblock.pred) block;
                block.succ := insertT !(block.succ) nextblock;
              end
            else
              let from, pred = !(block.from), !(block.pred) in
              remove blocks !idx;
              let () = pred |> List.iter @@ fun (T p) ->
                match Util.find_opt_with_index (fun (T blk) -> !(blk.from) = from) !(p.succ) with
                | Some (blk, i) ->
                  remove p.succ i;
                  p.succ := insertT !(p.succ) nextblock
                | None -> ()
              in
              begin
                nextblock.from := from;
                nextblock.pred := pred;
              end
          end
        else
          incr idx;
    done
  in
  blocks

type t = {
  from: int;
  _to: int;
  pred: int list;
  succ: int list
}
and fullcfg = Cfg of t list * fullcfg list
[@@deriving show {with_path = false }]


let rec from_t0_to_t ts =
  ts |> List.map @@ fun (s : t0) ->
  {
    from = !(s.from);
    _to = !(s._to);
    pred = !(s.pred) |> List.map (fun (T (s : t0)) ->
        let _, i = unwrap_option @@ Util.find_opt_with_index (fun (t : t0) -> !(t.from) = !(s.from)) ts in
        i
      );
    succ = !(s.succ) |> List.map (fun (T (s : t0)) ->
        let _, i = unwrap_option @@ Util.find_opt_with_index (fun (t : t0) -> !(t.from) = !(s.from)) ts in
        i
      )
  }

let rec mkcfg (Q(is, _, cls, _)) =
  let cfg = mkcfg0 is in
  Cfg(from_t0_to_t !cfg, List.map mkcfg cls)

