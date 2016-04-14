
type sbb_rr = [
  | `SBB64rr
  | `SBB32rr
  | `SBB16rr
  | `SBB8rr
] [@@deriving sexp]

type sbb_rm = [
  | `SBB64rm
  | `SBB32rm
  | `SBB16rm
  | `SBB8rm
] [@@deriving sexp]

type sbb_rax = [
  | `SBB64i32
  | `SBB32i32
  | `SBB16i16
  | `SBB8i8
] [@@deriving sexp]

type sbb_ri8 = [
  | `SBB64ri8
  | `SBB32ri8
  | `SBB16ri8
] [@@deriving sexp]

type sbb_ri = [
  | `SBB32ri
  | `SBB16ri
  | `SBB8ri
] [@@deriving sexp]

type sbb_reg = [ sbb_rr | sbb_rm | sbb_rax | sbb_ri8 | sbb_ri] [@@deriving sexp]

type sbb_mr = [
  | `SBB64mr
  | `SBB32mr
  | `SBB16mr
  | `SBB8mr
] [@@deriving sexp]

type sbb_mi8 = [
  | `SBB64mi8
  | `SBB32mi8
  | `SBB16mi8
] [@@deriving sexp]

type sbb_mi = [
  | `SBB32mi
  | `SBB16mi
  | `SBB8mi 
] [@@deriving sexp]

type sbb_mem = [ sbb_mr | sbb_mi8 | sbb_mi ] [@@deriving sexp]

type sbb64_i32 = [
  | `SBB64mi32
  | `SBB64ri32
] [@@deriving sexp]

type sbbx = [ sbb_reg | sbb_mem | sbb64_i32] [@@deriving sexp]
