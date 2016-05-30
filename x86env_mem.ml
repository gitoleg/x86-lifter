open Core_kernel.Std
open Bap.Std
open X86env_types

module Make (CPU : X86CPU) (RR : RR) (IM : IM) : MM = struct

  let addr_size = Arch.addr_size (CPU.arch :> arch)
  module Segment = struct
    module Base = struct
      type t =
        | GPR of RR.t
        | IP of word
        [@@ deriving variants, sexp]
      let create mem base =
        match Asm.Reg.decode base |> Option.value_exn with
        | #Asm.reg -> RR.of_mc_exn base |> gpr
        | `IP | `EIP | `RIP -> Memory.max_addr mem |> Word.succ |> ip
        | b -> Error.failwiths "invalid base" b Asm.Reg.sexp_of_t

    end

    type t = {
      seg : RR.t option;
      base : Base.t;
      scale : int option;
      index : RR.t option;
      disp : int;
    } [@@ deriving fields, sexp]

    let create mem mo =
      let seg = match Asm.Reg.decode mo.Operand.seg with
        | None -> None
        | Some `FS -> RR.of_asm_exn `FS_BASE |> Option.some
        | Some `GS -> RR.of_asm_exn `GS_BASE |> Option.some
        | Some r -> Error.failwiths "invalid segment" r
                      Asm.Reg.sexp_of_t in
      Fields.create ~seg
        ~base:(Base.create mem mo.Operand.base)
        ~scale:(Imm.to_int mo.Operand.scale)
        ~index:(RR.of_mc mo.Operand.index)
        ~disp:(Imm.to_int mo.Operand.disp |> Option.value_exn)

    let make_value reg =
      let size = Size.in_bits addr_size in
      let open Asm in
      match RR.to_asm reg, CPU.arch with
      | #Reg.r8, _
      | #Reg.r16, _
      | #Reg.r32, `x86_64 -> (RR.get reg |> Bil.(cast unsigned size))
      | #Reg.r32, `x86
      | #Reg.r64, _
      | #Reg.segment_base, _ -> RR.get reg

    let make_scale scale =
      let shift = match scale with
        | 1 -> None
        | 2 -> Some 1
        | 4 -> Some 2
        | 8 -> Some 3
        | s -> Error.failwiths "invalid memory scale" s sexp_of_int in
      Option.map ~f:(fun s -> Word.of_int ~width:2 s |> Bil.int) shift

    let make_disp disp =
      Option.some_if (disp <> 0) disp |>
      Option.map ~f:(fun disp ->
          Word.of_int ~width:(Size.in_bits addr_size) disp |>
          Bil.int)

    let addr {seg; base; scale; index; disp} =
      let seg = Option.map ~f:(fun seg -> make_value seg) seg in
      let base = match base with
        | Base.GPR base -> make_value base |> Option.some
        | Base.IP word -> Bil.int word |> Option.some in
      let scale = Option.value_map
          ~default:None
          ~f:make_scale scale in
      let index = Option.map ~f:make_value index in
      let disp = make_disp disp in

      let ( + ) op1 op2 = match op1, op2 with
        | Some op1, Some op2 -> Bil.(op1 + op2) |> Option.some
        | Some _ as op, None -> op
        | None, (Some _ as op) -> op
        | None, None -> None in

      let ( * ) op1 op2 = match op1, op2 with
        | Some op1, Some op2 -> Bil.(op2 lsl op1) |> Option.some
        | None, (Some _ as op) -> op
        | _, None -> None in
      Option.value_exn (seg + base + scale*index + disp)
  end

  module Relative = struct
    type t = {
      pc : addr;
      off : IM.t;
    } [@@ deriving fields]

    let create mem off =
      Fields.create
        ~pc:(Memory.max_addr mem |> Word.succ)
        ~off:(IM.of_imm off)

    let addr {pc; off} =
      let off = IM.get off ~width:(addr_size :> size) in
      Bil.(int pc + off)
  end

  type t =
    | Segment of Segment.t
    | Relative of Relative.t
    [@@ deriving variants]


  let of_mem mem mo =
    Segment.create mem mo |> segment

  let of_offset mem imm =
    Relative.create mem imm |> relative

  let addr =
    Variants.map ~segment:(fun _ -> Segment.addr)
      ~relative:(fun _ -> Relative.addr)

  let load_from addr ~size =
    let mem = Bil.var CPU.mem in
    Bil.load ~mem ~addr LittleEndian size

  let store_to addr ~size data =
    let mem = Bil.var CPU.mem in
    Bil.(CPU.mem := store ~mem ~addr data LittleEndian size)

  let load t ~size =  addr t |> load_from ~size

  let store t ~size data = addr t |> fun addr -> store_to ~size addr data
end
