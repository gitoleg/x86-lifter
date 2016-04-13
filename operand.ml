open Core_kernel.Std
open Bap.Std

type nonrec reg = reg

type nonrec imm = imm

type mem = {
  seg : reg;
  base : reg;
  scale : imm;
  index : reg;
  disp : imm
}

module Decoder = struct
  type 'a t = (op list -> ('a * op list) option)


  let r = function
    | (Op.Reg reg)::other -> Some (reg, other)
    | _ -> None

  let i = function
    | (Op.Imm imm)::other -> Some (imm, other)
    | _ -> None

  let m = function
    | (Op.Reg base)::(Op.Imm scale)::(Op.Reg index)::
      (Op.Imm disp)::(Op.Reg seg)::other ->
      Some ({seg; base; scale; index; disp}, other)
    | _ -> None

  let finish ops result =
    match ops with
    | [] -> Some result
    | _ -> None

  let unary t ops =
    let open Option in
    Array.to_list ops |>
    t >>= fun (op, ops) -> finish ops op

  let binary t1 t2 ops =
    let open Option in
    Array.to_list ops |>
    t1 >>= fun (op1, ops) ->
    t2 ops >>= fun (op2, ops) ->
    finish ops (op1, op2)

  let ternary t1 t2 t3 ops =
    let open Option in
    Array.to_list ops |>
    t1 >>= fun (op1, ops) ->
    t2 ops >>= fun (op2, ops) ->
    t3 ops >>= fun (op3, ops) ->
    finish ops (op1, op2, op3)
end


let rr = Decoder.(binary r r)
let ri = Decoder.(binary r i)
let rm = Decoder.(binary r m)
let mr = Decoder.(binary m r)
let mi = Decoder.(binary m i)

let rr_exn ops = rr ops |> Option.value_exn
let ri_exn ops = ri ops |> Option.value_exn
let rm_exn ops = rm ops |> Option.value_exn
let mr_exn ops = mr ops |> Option.value_exn
let mi_exn ops = mi ops |> Option.value_exn

let r = Decoder.(unary r)
let i = Decoder.(unary i)
let m = Decoder.(unary m)

let r_exn ops = r ops |> Option.value_exn
let i_exn ops = i ops |> Option.value_exn
let m_exn ops = m ops |> Option.value_exn


