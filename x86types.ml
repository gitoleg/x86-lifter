open Bap.Std

(** 8-bit low byte GPR registers *)
type r8l = [
  | `AL | `BL | `CL | `DL
  | `SIL | `DIL | `BPL | `SPL
  | `R8B | `R9B | `R10B | `R11B
  | `R12B | `R13B | `R14B | `R15B
] [@@deriving sexp]

(** 8-bit high-byte GPR registers *)
type r8h = [`AH | `BH | `CH | `DH ] [@@deriving sexp]

(** all 8 bit GPR registers *)
type r8 = [r8l | r8h] [@@deriving sexp]

(** 16-bit GPR registers *)
type r16 = [
  |`AX | `BX | `CX | `DX
  | `DI | `SI | `BP | `SP
  | `R8W | `R9W | `R10W | `R11W
  | `R12W | `R13W | `R14W | `R15W
] [@@deriving sexp]

(** 32-bit GPR registers *)
type r32 = [
  | `EAX | `EBX | `ECX | `EDX
  | `EDI | `ESI | `EBP | `ESP
  | `R8D | `R9D | `R10D | `R11D
  | `R12D | `R13D | `R14D | `R15D
] [@@deriving sexp]

(** 64-bit GPR registers *)
type r64 = [
  | `RAX | `RBX | `RCX | `RDX
  | `RDI | `RSI | `RBP | `RSP
  | `R8 | `R9 | `R10 | `R11
  | `R12 | `R13 | `R14 | `R15
] [@@deriving sexp]

type x86reg = [
  | `Nil (* a special case, when no register value specify *)
  | r8
  | r16
  | r32
  | r64
] [@@deriving sexp]

module type Env = sig
  (** Register representation *)
  module RR : sig
    val of_reg : reg -> x86reg
    val var : x86reg -> var
    val size : [`r32 | `r64] Size.p (* GPR size *)
    val width : x86reg -> size
    val bitwidth : x86reg -> int
    val get : x86reg -> exp
    val set : x86reg -> exp -> stmt
  end

  (** Memory model *)
  module MM : sig
    type t
    val from_addr : seg:reg
      -> base:reg
      -> scale:imm
      -> index:reg
      -> disp:imm -> t

    val addr : t -> exp
    val load : t -> size -> exp
    val store : t -> size -> exp -> stmt
  end
end

module type Registrable = sig
  val register: unit -> unit list Core_kernel.Std.Or_error.t
end
