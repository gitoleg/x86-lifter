open Core_kernel.Std
open Bap.Std
open Cmpxchg_opcode
open Asm.Reg

module Make (Env : X86env.S) = struct
  open Env

  let local_var name width =
    Var.create ~is_virtual:true ~fresh:true name @@ Type.imm width

  let get_rax = function
    | `CMPXCHG8rr
    | `CMPXCHG8rm -> `AL
    | `CMPXCHG16rr
    | `CMPXCHG16rm -> `AX
    | `CMPXCHG32rr
    | `CMPXCHG32rm -> `EAX
    | `CMPXCHG64rr
    | `CMPXCHG64rm -> `RAX

  let cmpxchg_rr (op:cmpxchg_rr) _mem ops =
    Operand.rr ops ~f:(fun fst snd ->
        let fst = RR.of_mc_exn fst in
        let snd = RR.of_mc_exn snd in
        let rax = get_rax op |> RR.of_asm_exn in
        let size = RR.width rax in
        let v = local_var "v" @@ Size.in_bits size in
        let bil =
          let sub = Bil.(v := RR.get rax - RR.get fst) in
          let flags = FR.after_sub ~diff:(Bil.var v)
              ~op1:(RR.get rax) ~op2:(RR.get fst) size in
          let cmpxchg = Bil.(if_ (RR.get rax = RR.get fst)
                               ([RR.get snd |> RR.set fst])
                               ([RR.get fst |> RR.set rax])) in
          List.concat [sub::flags; [cmpxchg]] in
        Ok bil)

  let cmpxchg_rm (op:cmpxchg_rm) mem ops =
    Operand.mr ops ~f:(fun mo reg ->
        let fst = MM.of_mem mem mo in
        let snd = RR.of_mc_exn reg in
        let rax = get_rax op |> RR.of_asm_exn in
        let size = RR.width rax in
        let bits_size = Size.in_bits size in
        let d = local_var "d" bits_size in
        let v = local_var "v" bits_size in
        let bil =
          let load = Bil.(d := MM.load fst ~size) in
          let sub = Bil.(v := RR.get rax - var d) in
          let flags = FR.after_sub ~diff:(Bil.var v)
              ~op1:(RR.get rax) ~op2:(Bil.var d) size in
          let cmpxchg = Bil.(if_ (RR.get rax = var d)
                               ([RR.get snd |> MM.store fst ~size])
                               ([RR.set rax (Bil.var d)])) in
          List.concat[load::sub::flags; [cmpxchg]] in
        Ok bil)

  let cmpxchg (op:cmpxchg) =
    match op with
    | #cmpxchg_rr as op -> cmpxchg_rr op
    | #cmpxchg_rm as op -> cmpxchg_rm op
end

module IA32 = Make (X86env.IA32)
module AMD64 = Make (X86env.AMD64)

let register () =
  List.iter (all_of_cmpxchg_ia32 :> cmpxchg list) ~f:(fun op ->
      X86backend.IA32.register (op :> Opcode.t) (IA32.cmpxchg op));
  List.iter (all_of_cmpxchg :> cmpxchg list) ~f:(fun op ->
      X86backend.AMD64.register (op :> Opcode.t) AMD64.(cmpxchg op));
