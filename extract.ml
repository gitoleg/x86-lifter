open Core_kernel.Std
open Bap.Std
open X86types

module type S = sig
  type reg
  type imm
  type mem
  val ur : op array -> reg option
  val ui : op array -> imm option
  val um : op array -> mem option
  val brr : op array -> (reg * reg) option
  val bri : op array -> (reg * imm) option
  val brm : op array -> (reg * mem) option
  val bmr : op array -> (mem * reg) option
  val bmi : op array -> (mem * imm) option
end

module Make (Env : Env) : S
  with type reg = x86reg
  with type imm = imm
  with type mem = Env.MM.t = struct
  open Env
  type reg = x86reg
  type imm = Bap.Std.imm
  type mem = Env.MM.t

  let reg = function
    | (Op.Reg reg)::other ->
      Some (RR.of_reg reg, other)
    | _ -> None

  let imm = function
    | (Op.Imm imm)::other -> Some (imm, other)
    | _ -> None

  let mem = function
    | (Op.Reg base)::(Op.Imm scale)::(Op.Reg index)::
      (Op.Imm disp)::(Op.Reg seg)::other ->
      Some (MM.from_addr ~seg ~base ~scale ~index ~disp, other)
    | _ -> None

  let extract_unary fn ops =
    let open Option in
    Array.to_list ops |>
    fn >>=
    fun (op, other) -> some_if (List.is_empty other) op

  let ur = extract_unary reg
  let ui = extract_unary imm
  let um = extract_unary mem

  let extract_binary fn1 fn2 ops =
    let open Option in
    Array.to_list ops |>
    fn1 >>=
    fun (op1, ops) -> fn2 ops >>=
    fun (op2, ops) -> some_if (List.is_empty ops) (op1, op2)

  let brr = extract_binary reg reg
  let bri = extract_binary reg imm
  let brm = extract_binary reg mem
  let bmr = extract_binary mem reg
  let bmi = extract_binary mem imm
end
