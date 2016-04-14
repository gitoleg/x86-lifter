
type sub_rr = [
  | `SUB64rr
  | `SUB32rr
  | `SUB16rr
  | `SUB8rr
] [@@deriving sexp]

type sub_rm = [
  | `SUB64rm
  | `SUB32rm
  | `SUB16rm
  | `SUB8rm
] [@@deriving sexp]

type sub_rax = [
  | `SUB64i32
  | `SUB32i32
  | `SUB16i16
  | `SUB8i8
] [@@deriving sexp]

type sub_ri8 = [
  | `SUB64ri8
  | `SUB32ri8
  | `SUB16ri8
] [@@deriving sexp]

type sub_ri = [
  | `SUB32ri
  | `SUB16ri
  | `SUB8ri
] [@@deriving sexp]

type sub_reg = [ sub_rr | sub_rm | sub_rax | sub_ri8 | sub_ri] [@@deriving sexp]

type sub_mr = [
  | `SUB64mr
  | `SUB32mr
  | `SUB16mr
  | `SUB8mr
] [@@deriving sexp]

type sub_mi8 = [
  | `SUB64mi8
  | `SUB32mi8
  | `SUB16mi8
] [@@deriving sexp]

type sub_mi = [
  | `SUB32mi
  | `SUB16mi
  | `SUB8mi
] [@@deriving sexp]

type sub_mem = [ sub_mr | sub_mi8 | sub_mi ] [@@deriving sexp]

type sub64_i32 = [
  | `SUB64mi32
  | `SUB64ri32
] [@@deriving sexp]

type subx = [ sub_reg | sub_mem | sub64_i32] [@@deriving sexp]

