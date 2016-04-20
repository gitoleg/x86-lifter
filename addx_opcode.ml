
type add_rr = [
  | `ADD64rr
  | `ADD32rr
  | `ADD16rr
  | `ADD8rr
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rm = [
  | `ADD64rm
  | `ADD32rm
  | `ADD16rm
  | `ADD8rm
] [@@deriving bin_io, compare, enumerate, sexp]

type add_rax = [
  | `ADD64i32
  | `ADD32i32
  | `ADD16i16
  | `ADD8i8
] [@@deriving bin_io, compare, enumerate, sexp]

type add_ri = [
  | `ADD32ri
  | `ADD16ri
  | `ADD8ri 
]  [@@deriving bin_io, compare, enumerate, sexp]

type add_ri_width = [
  | `ADD64ri8
  | `ADD32ri8
  | `ADD16ri8
  | `ADD64ri32
] [@@deriving bin_io, compare, enumerate, sexp]

type add_reg = [ add_rr | add_rm | add_rax | add_ri | add_ri_width]
[@@deriving bin_io, compare, enumerate, sexp]

type add_mr = [
  | `ADD64mr
  | `ADD32mr
  | `ADD16mr
  | `ADD8mr
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mi = [
  | `ADD32mi
  | `ADD16mi
  | `ADD8mi 
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mi_width = [
  | `ADD64mi8
  | `ADD32mi8
  | `ADD16mi8
  | `ADD64mi32
] [@@deriving bin_io, compare, enumerate, sexp]

type add_mem = [ add_mr | add_mi | add_mi_width ]
  [@@deriving bin_io, compare, enumerate, sexp]

type t = [ add_reg | add_mem ]
 [@@deriving bin_io, compare, enumerate, sexp]
