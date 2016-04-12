

type add_rr = [
  | `ADD64rr
  | `ADD32rr
  | `ADD16rr
  | `ADD8rr
] [@@deriving sexp]

type add_rm = [
  | `ADD64rm
  | `ADD32rm
  | `ADD16rm
  | `ADD8rm
] [@@deriving sexp]

(** TODO: thus looks like subset of different add*ri where reg = *ax  *)
type add_rax = [
  | `ADD64i32
  | `ADD32i32
  | `ADD16i16
  | `ADD8i8
] [@@deriving sexp]

type add_ri8 = [
  | `ADD64ri8
  | `ADD32ri8
  | `ADD16ri8
] [@@deriving sexp]

type add_ri = [
  | `ADD32ri
  | `ADD16ri
  | `ADD8ri
] [@@deriving sexp]

type add_reg = [ add_rr | add_rm | add_rax | add_ri8 | add_ri] [@@deriving sexp]

type add_mr = [
  | `ADD64mr
  | `ADD32mr
  | `ADD16mr
  | `ADD8mr
] [@@deriving sexp]

type add_mi8 = [
  | `ADD64mi8
  | `ADD32mi8
  | `ADD16mi8
] [@@deriving sexp]

type add_mi = [
  | `ADD32mi
  | `ADD16mi
  | `ADD8mi 
] [@@deriving sexp]

type add_mem = [ add_mr | add_mi8 | add_mi ] [@@deriving sexp]

type add64_i32 = [
  | `ADD64mi32
  | `ADD64ri32
] [@@deriving sexp]

type addx = [ add_reg | add_mem | add64_i32] [@@deriving sexp]
