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


module Unary = struct
  type ('a, 'b) action = ('a -> 'b Or_error.t) -> 'b Or_error.t
  type 'a arg = op array -> 'a option
  type ('a, 'b) parser = 'a arg -> op array -> ('a, 'b) action

  module Arg = struct
    type 'a t = 'a arg
    let reg = function
      | [| Op.Reg reg |] -> Some reg
      | _ -> None

    let imm = function
      | [| Op.Imm imm |] -> Some imm
      | _ -> None

    let mem = function
      | [| Op.Reg base; Op.Imm scale; Op.Reg index;
           Op.Imm disp; Op.Reg seg|] ->
        Some ({seg; base; scale; index; disp})
      | _ -> None
  end

  let run : ('a, 'b) parser = fun arg ops f -> match arg ops with
    | Some op -> f op
    | None -> Or_error.error_string "invalid operands"
end

module Binary = struct
  type ('a, 'b, 'c) action = ('a -> 'b -> 'c Or_error.t) -> 'c Or_error.t
  type ('a, 'b) arg = op array -> ('a * 'b) option
  type ('a, 'b, 'c) parser = ('a, 'b) arg -> op array -> ('a, 'b, 'c) action

  module Arg = struct
    type ('a, 'b) t = ('a, 'b) arg
    let reg_reg = function
      | [| Op.Reg reg1; Op.Reg reg2 |] -> Some (reg1, reg2)
      | _ -> None

    let reg_imm = function
      | [| Op.Reg reg; Op.Imm imm |] -> Some (reg, imm)
      | _ -> None

    let reg_mem = function
      | [| Op.Reg reg; Op.Reg base; Op.Imm scale; Op.Reg index;
           Op.Imm disp; Op.Reg seg|] ->
        Some (reg, {seg; base; scale; index; disp})
      | _ -> None

    let mem_reg = function
      | [| Op.Reg base; Op.Imm scale; Op.Reg index;
           Op.Imm disp; Op.Reg seg; Op.Reg reg |] ->
        Some ({seg; base; scale; index; disp}, reg)
      | _ -> None

    let mem_imm = function
      | [| Op.Reg base; Op.Imm scale; Op.Reg index;
           Op.Imm disp; Op.Reg seg; Op.Imm imm |] ->
        Some ({seg; base; scale; index; disp}, imm)
      | _ -> None

  end

  let run : ('a, 'b, 'c) parser = fun arg ops f -> match arg ops with
    | Some (op1, op2) -> f op1 op2
    | None -> Or_error.error_string "invalid operands"
end

let r ops ~f = Unary.(run Arg.reg ops f)
let i ops ~f = Unary.(run Arg.imm ops f)
let m ops ~f = Unary.(run Arg.mem ops f)

let rr ops ~f = Binary.(run Arg.reg_reg ops f)
let ri ops ~f = Binary.(run Arg.reg_imm ops f)
let rm ops ~f = Binary.(run Arg.reg_mem ops f)
let mr ops ~f = Binary.(run Arg.mem_reg ops f)
let mi ops ~f = Binary.(run Arg.mem_imm ops f)
