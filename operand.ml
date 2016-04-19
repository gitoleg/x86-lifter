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
  type ('a, 'b) action = 'a -> 'b Or_error.t
  type ('a, 'b) parser = ('a, 'b) action -> op array -> 'b Or_error.t

  let fail () = Or_error.error_string "invalid arg"

  let reg f = function
    | [| Op.Reg reg |] -> f reg
    | _ -> fail ()

  let imm f = function
    | [| Op.Imm imm |] -> f imm
    | _ -> fail ()

  let mem f = function
    | [| Op.Reg base; Op.Imm scale; Op.Reg index;
         Op.Imm disp; Op.Reg seg|] ->
      f {seg; base; scale; index; disp}
    | _ -> fail ()
end

module Binary = struct
  type ('a, 'b, 'c) action = 'a -> 'b -> 'c Or_error.t
  type ('a, 'b, 'c) parser = ('a, 'b, 'c) action -> op array -> 'c Or_error.t

  let fail () = Or_error.error_string "invalid arg"

  let reg_reg f = function
    | [| Op.Reg reg1; Op.Reg reg2 |] -> f reg1 reg2
    | _ -> fail ()

  let reg_imm f = function
    | [| Op.Reg reg; Op.Imm imm |] -> f reg imm
    | _ -> fail ()

  let reg_mem f = function
    | [| Op.Reg reg; Op.Reg base; Op.Imm scale; Op.Reg index;
         Op.Imm disp; Op.Reg seg|] ->
      f reg {seg; base; scale; index; disp}
    | _ -> fail ()

  let mem_reg f = function
    | [| Op.Reg base; Op.Imm scale; Op.Reg index;
         Op.Imm disp; Op.Reg seg; Op.Reg reg |] ->
      f {seg; base; scale; index; disp} reg
    | _ -> fail ()

  let mem_imm f = function
    | [| Op.Reg base; Op.Imm scale; Op.Reg index;
         Op.Imm disp; Op.Reg seg; Op.Imm imm |] ->
      f {seg; base; scale; index; disp} imm
    | _ -> fail ()

end

module Ternary = struct
  type ('a, 'b, 'c, 'd) action = 'a -> 'b -> 'c -> 'd Or_error.t
  type ('a, 'b, 'c, 'd) parser =
    ('a, 'b, 'c, 'd) action -> op array -> 'd Or_error.t

  let fail () = Or_error.error_string "invalid arg"

  let reg_reg_reg f = function
    | [| Op.Reg reg1; Op.Reg reg2; Op.Reg reg3 |] -> f reg1 reg2 reg3
    | _ -> fail ()

  let reg_reg_imm f = function
    | [| Op.Reg reg1; Op.Reg reg2; Op.Imm imm |] -> f reg1 reg2 imm
    | _ -> fail ()

  let reg_reg_mem f = function
    | [| Op.Reg reg1; Op.Reg reg2; Op.Reg base; Op.Imm scale;
         Op.Reg index; Op.Imm disp; Op.Reg seg|] ->
      f reg1 reg2 {seg; base; scale; index; disp}
    | _ -> fail ()

end
let r ops ~f = Unary.reg f ops
let i ops ~f = Unary.imm f ops
let m ops ~f = Unary.mem f ops

let rr ops ~f = Binary.reg_reg f ops
let ri ops ~f = Binary.reg_imm f ops
let rm ops ~f = Binary.reg_mem f ops
let mr ops ~f = Binary.mem_reg f ops
let mi ops ~f = Binary.mem_imm f ops

let rrr ops ~f = Ternary.reg_reg_reg f ops
let rri ops ~f = Ternary.reg_reg_imm f ops
let rrm ops ~f = Ternary.reg_reg_mem f ops
