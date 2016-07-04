
open Bap.Std

module IA32 : Target = struct
  module Old = (val (target_of_arch `x86))
  module CPU = Old.CPU
  let lift mem insn = 
    List.hd (Lift.amd64 ~on_unsupported:Old.lift [mem, insn])
end

module AMD64 : Target = struct
  module Old = (val (target_of_arch `x86_64))
  module CPU = Old.CPU
  let lift mem insn = 
    List.hd (Lift.amd64 ~on_unsupported:Old.lift [mem, insn])
end

let () =
  register_target `x86 (module IA32);
  register_target `x86_64 (module AMD64)
