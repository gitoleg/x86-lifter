
module Reg = Asm_reg

type reg = [Reg.gpr | Reg.segment_base] [@@deriving sexp_poly]
