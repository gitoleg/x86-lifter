
type add_rr32 = [
  | `ADD32rr
  | `ADD16rr
  | `ADD8rr
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rr = [
  | add_rr32
  | `ADD64rr
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rm32 = [
  | `ADD32rm
  | `ADD16rm
  | `ADD8rm
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rm = [
  | add_rm32
  | `ADD64rm
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rax32 = [
  | `ADD32i32
  | `ADD16i16
  | `ADD8i8
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rax = [
  | add_rax32
  | `ADD64i32
] [@@deriving bin_io, compare, enumerate, sexp]

type add_ri = [
  | `ADD32ri
  | `ADD16ri
  | `ADD8ri 
] [@@deriving bin_io, compare, enumerate, sexp]

type add_ri32_width = [
  | `ADD32ri8
  | `ADD16ri8
] [@@deriving bin_io, compare, enumerate, sexp]

type add_ri_width = [
  | add_ri32_width
  | `ADD64ri32
  | `ADD64ri8
] [@@deriving bin_io, compare, enumerate, sexp]

type add_reg32 = [ add_rr32 | add_rm32 | add_rax32 | add_ri | add_ri32_width]
  [@@deriving bin_io, compare, enumerate, sexp]

type add_reg = [ add_rr | add_rm | add_rax | add_ri | add_ri_width]
  [@@deriving bin_io, compare, enumerate, sexp]

type add_mr32 = [
  | `ADD32mr
  | `ADD16mr
  | `ADD8mr
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mr = [
  | add_mr32
  | `ADD64mr
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mi = [
  | `ADD32mi
  | `ADD16mi
  | `ADD8mi 
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mi32_width = [
  | `ADD32mi8
  | `ADD16mi8
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mi_width = [
  | add_mi32_width
  | `ADD64mi32
  | `ADD64mi8
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mem32 = [ add_mr32 | add_mi | add_mi32_width ]
  [@@deriving bin_io, compare, enumerate, sexp]

type add_mem = [ add_mr | add_mi | add_mi_width ]
  [@@deriving bin_io, compare, enumerate, sexp]

type t32 = [ add_reg32 | add_mem32 ]
  [@@deriving bin_io, compare, enumerate, sexp]

type t64 = [ add_reg | add_mem ]
  [@@deriving bin_io, compare, enumerate, sexp]
