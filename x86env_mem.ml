open Core_kernel.Std
open Bap.Std
open X86env_types

module Make (CPU : X86CPU) (RR : RR) (IM : IM) : MM = struct
  module Segment = struct
    type t = {
      seg : RR.t option;
      base : RR.t;
      scale : int option;
      index : RR.t option;
      disp : int;
    } [@@ deriving fields, sexp]

    let create mem =
      Fields.create ~seg:(RR.of_mc mem.Operand.seg)
        ~base:(RR.of_mc mem.Operand.base |> Option.value_exn)
        ~scale:(Imm.to_int mem.Operand.scale)
        ~index:(RR.of_mc mem.Operand.index)
        ~disp:(Imm.to_int mem.Operand.disp |> Option.value_exn)

    let make_value reg =
      let size = Arch.addr_size (CPU.arch :> arch) |>
                 Size.in_bits in
      let open Asm in
      match RR.to_asm reg, CPU.arch with
      | #Reg.r8, _
      | #Reg.r16, _
      | #Reg.r32, `x86_64 -> (RR.get reg |> Bil.(cast unsigned size))
      | #Reg.r32, `x86
      | #Reg.r64, _ -> RR.get reg

    let make_scale scale =
      let shift = match scale with
        | 1 -> None
        | 2 -> Some 1
        | 4 -> Some 2
        | 8 -> Some 3
        | s -> Error.failwiths "invalid memory scale" s sexp_of_int in
      Option.map ~f:(fun s -> Word.of_int ~width:2 s |> Bil.int) shift

    let addr ({seg; base; scale; index; disp} as t) =
      let seg = Option.map ~f:(fun s ->
          Error.failwiths "segment memory model not implemented yet"
            s RR.sexp_of_t) seg in
      let base = make_value base |> Option.some in
      let scale = Option.value_map
          ~default:None
          ~f:make_scale scale in
      let index = Option.map ~f:make_value index in
      let disp = None in

      let ( + ) op1 op2 = match op1, op2 with
        | Some op1, Some op2 -> Bil.(op1 + op2) |> Option.some
        | Some _ as op, None -> op
        | None, (Some _ as op) -> op
        | None, None -> None in

      let ( * ) op1 op2 = match op1, op2 with
        | Some op1, Some op2 -> Bil.(op2 lsl op1) |> Option.some
        | None, (Some _ as op) -> op
        | _ -> Error.failwiths "invalid segment" t sexp_of_t in
      Option.value_exn (seg + base + scale*index + disp)
  end

  module Absolute = struct
    type t = {
      pc : addr option;
      off : IM.t;
    } [@@ deriving fields]

    let create mem off =
      Fields.create
        ~pc:(match CPU.arch with
              | `x86 -> Memory.max_addr mem |> Option.some
              | `x86_64 -> None)
        ~off:(IM.of_imm off)

    let addr {pc; off} =
      let size = Arch.addr_size (CPU.arch :> arch) in
      let off = IM.get off ~width:(size :> size) in
      match pc with
      | Some pc -> Bil.(int pc + off)
      | None -> off
  end

  type t =
    | Segment of Segment.t
    | Absolute of Absolute.t
    [@@ deriving variants]


  let of_mem mem =
    Segment.create mem |> segment

  let of_offset mem imm =
    Absolute.create mem imm |> absolute

  let addr =
    Variants.map ~segment:(fun _ -> Segment.addr)
      ~absolute:(fun _ -> Absolute.addr)

  let load t ~size =
    let addr = addr t in
    let mem = Bil.var CPU.mem in
    Bil.load ~mem ~addr LittleEndian size

  let store t ~size data =
    let addr = addr t in
    let mem = Bil.var CPU.mem in
    Bil.(CPU.mem := store ~mem ~addr data LittleEndian size)
end
