open Core_kernel.Std
open Bap.Std

module type S = sig
  (** Register representation *)
  module RR : sig
    type t
    val of_reg : Operand.reg -> t option
    val of_reg_exn : Operand.reg -> t
    val to_x86reg : t -> X86reg.t
    val width : t -> [`r8 | `r16 | `r32 | `r64] Size.p
    val var : t -> var
    val size : [`r32 | `r64] Size.p (* GPR size *)
    val get : t -> exp
    val set : t -> exp -> stmt
  end

  (** Memory model *)
  module MM : sig
    type t
    val of_mem : Operand.mem -> t
    val addr : t -> exp
    val load : t -> size -> exp
    val store : t -> size -> exp -> stmt
  end

  (**Imm model*)
  module IM : sig
    type t
    val of_imm : Operand.imm -> t
    val get : width:([`r8 | `r16 | `r32 | `r64] Size.p) -> t -> exp
  end
end

module type X86CPU = sig
 type regs = private [< X86reg.t]
 val arch : Arch.x86
 val avaliable : X86reg.t -> bool
 include module type of X86_cpu.AMD64
end

module Make(CPU : X86CPU) : S = struct
  module RR = struct
    type t = X86reg.t [@@deriving sexp]

    let of_reg_exn reg =
      match X86reg.decode reg with
      | Some r when CPU.avaliable r -> r
      | Some _ | None ->
        Error.failwiths "invalid reg" reg sexp_of_reg

    let of_reg reg = Option.try_with (fun () -> of_reg_exn reg)

    let to_x86reg t = t

    let width = X86reg.width

    let var_all = function
      | `AL | `AH | `AX | `EAX | `RAX -> CPU.rax
      | `DL | `DH | `DX | `EDX | `RDX -> CPU.rdx
      | `CL | `CH | `CX | `ECX | `RCX -> CPU.rcx
      | `BL | `BH | `BX | `EBX | `RBX -> CPU.rbx
      | `DIL | `DI | `EDI | `RDI -> CPU.rdi
      | `BPL | `BP | `EBP | `RBP -> CPU.rbp
      | `SPL | `SP | `ESP | `RSP -> CPU.rsp
      | `SIL | `SI | `ESI | `RSI -> CPU.rsi
      | `R8B | `R8W | `R8D | `R8 -> CPU.r.(0)
      | `R9B | `R9W | `R9D | `R9 -> CPU.r.(1)
      | `R10B | `R10W | `R10D | `R10 -> CPU.r.(2)
      | `R11B | `R11W | `R11D | `R11 -> CPU.r.(3)
      | `R12B | `R12W | `R12D | `R12 -> CPU.r.(4)
      | `R13B | `R13W | `R13D | `R13 -> CPU.r.(5)
      | `R14B | `R14W | `R14D | `R14 -> CPU.r.(6)
      | `R15B | `R15W | `R15D | `R15 -> CPU.r.(7)

    let var t =
      if CPU.avaliable t then var_all t
      else Error.failwiths "invalid reg variable"
          t X86reg.sexp_of_t

    let size = match CPU.arch with `x86 -> `r32 | `x86_64 -> `r64

    let bitsize = size |> Size.in_bits

    let bvar r = var r |> Bil.var

    let get r =
      let open X86reg in
      let v = bvar r in
      match r, CPU.arch with
      | #r8h, _ -> Bil.(extract ~hi:8 ~lo:15 v)
      | #r8l, _ | #r16, _ | #r32, `x86_64 ->
        Bil.(cast low (bitwidth r) v)
      | #r32, `x86 | #r64, _ -> v

    let set r e =
      let open X86reg in
      let lhs = var r in
      let rhs =
        let v = bvar r in
        match r, CPU.arch with
        | #r8h, _ ->
          let hp = bitsize - 16 in
          Bil.((cast high hp v) ^ e ^ (cast low 8 v))
        | #r8l, _ | #r16, _ ->
          let hp = bitsize - bitwidth r in
          Bil.((cast high hp v) ^ e)
        | #r32, `x86_64 -> Bil.(cast unsigned bitsize e)
        | #r32, `x86 | #r64, `x86_64 -> e
        | #r64, `x86 -> Error.failwiths "invalid reg"
                          r X86reg.sexp_of_t in
      Bil.(lhs := rhs)
  end

  module MM = struct
    type t = {
      seg : RR.t option;
      base : RR.t;
      scale : int;
      index : RR.t option;
      disp : int;
    } [@@ deriving fields]

    let of_mem mem =
      Fields.create ~seg:(RR.of_reg mem.Operand.seg)
        ~base:(RR.of_reg mem.Operand.base |> Option.value_exn)
        ~scale:(Imm.to_int mem.Operand.scale |> Option.value_exn)
        ~index:(RR.of_reg mem.Operand.index)
        ~disp:(Imm.to_int mem.Operand.disp |> Option.value_exn)

    let addr_size = Arch.addr_size (CPU.arch :> arch)

    let addr_bitsize = addr_size |> Size.in_bits

    let addr {seg; base; scale; index; disp} =
      let regval r =
        let open X86reg in
        match RR.to_x86reg r, CPU.arch with
        | #r8, _
        | #r16, _
        | #r32, `x86_64 ->
          let v = RR.get r in
          Bil.(cast unsigned addr_bitsize v)
        | #r32, `x86
        | #r64, _ -> RR.get r in
      let base = regval base in
      let scale =
        let make_scale value =
          Word.of_int ~width:2 value |> Bil.int |> Option.some in
        match scale with
        | 1 -> None
        | 2 -> make_scale 1
        | 4 -> make_scale 2
        | 8 -> make_scale 3
        | _ -> Error.failwiths "invalid address scale"
                 scale sexp_of_int in
      let index = Option.map ~f:regval index in
      let disp = match disp with
        | 0 -> None
        | _ -> Word.of_int ~width:addr_bitsize disp |>
               Bil.int |> Option.some in
      let addr =
        base |>
        (fun a -> match scale, index with
           | Some s, Some i -> Bil.(a + (i lsl s))
           | None, Some i -> Bil.(a + i)
           | _, None -> a) |>
        (fun a -> match disp with
           | None -> a
           | Some d -> Bil.(a + d)) in
      match seg with
      | None -> addr
      | Some s -> Error.failwiths
                    "segment memory model not implemented yet"
                    s RR.sexp_of_t

    let load t size =
      let addr = addr t in
      let mem = Bil.var CPU.mem in
      Bil.load ~mem ~addr LittleEndian size

    let store t size data =
      let addr = addr t in
      let mem = Bil.var CPU.mem in
      Bil.(CPU.mem := store ~mem ~addr data LittleEndian size)
  end

  module IM = struct
    type t = Operand.imm
    let of_imm imm = imm
    let get ~(width:[`r8 | `r16 | `r32 | `r64] Size.p) t =
      Imm.to_word t ~width:(Size.in_bits width) |>
      Option.value_exn |>
      Bil.int
  end
end

module IA32CPU : X86CPU = struct
 type regs = [
   | `AL | `BL | `CL | `DL
   | `AH | `BH | `CH | `DH
   | `AX | `BX | `CX | `DX
   | `DI | `SI | `BP | `SP
   | `EAX | `EBX | `ECX | `EDX
   | `EDI | `ESI | `EBP | `ESP
 ]

 let arch = `x86
 let avaliable = function
   | #regs -> true
   | _ -> false
 include X86_cpu.IA32
end

module AMD64CPU : X86CPU = struct
 type regs = X86reg.t

 let arch = `x86_64
 let avaliable = function
   | #regs -> true
   | _ -> false
 include X86_cpu.AMD64
end

module IA32 = Make(IA32CPU)
module AMD64 = Make(AMD64CPU)
