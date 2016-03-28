open Bap.Std

type low8 = [`AL | `DL | `CL | `BL ] [@@deriving sexp]
type high = [`AH | `DH | `CH | `BH ] [@@deriving sexp]
type low16 = [ `AX | `BX | `CX | `DX | `BP | `SP | `SI | `DI] [@@deriving sexp]

type x64_low8 = [`SIL | `DIL | `BPL | `SPL ] [@@deriving sexp]
type r8  = [`R8B | `R9B | `R10B | `R11B | `R12B | `R13B | `R14B | `R15B] [@@deriving sexp]
type r16 = [`R8W | `R9W | `R10W | `R11W | `R12W | `R13W | `R14W | `R15W] [@@deriving sexp]
type r32 = [`R8D | `R9D | `R10D | `R11D | `R12D | `R13D | `R14D | `R15D] [@@deriving sexp]

type reg32 = [
  | `EAX
  | `EDX
  | `ECX
  | `EBX
  | `ESI
  | `EDI
  | `EBP
  | `ESP
] [@@deriving sexp]

type reg64 = [
  | `RAX
  | `RDX
  | `RCX
  | `RBX
  | `RSI
  | `RDI
  | `RBP
  | `RSP
  | `R8
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15
] [@@deriving sexp]

type pseudo32 = [low8 | high | low16] [@@deriving sexp]
type any32 = [ pseudo32 | reg32] [@@deriving sexp]
type pseudo64 = [any32 | x64_low8 | r8 | r16 | r32 ] [@@deriving sexp]
type any64 = [pseudo64 | reg64] [@@deriving sexp]
type any = any64  [@@deriving sexp]

module type X86 = module type of X86_cpu.AMD64
