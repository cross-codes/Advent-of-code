namespace AOC2019.Solutions.Shared.IntcodeComputer

open ParameterMode

module Contexts =
    type public Context =
        { Memory: int array
          mutable Pointer: int
          FixedInput: int option }

    let public interpretParameter context mode rawParameter =
        match mode with
        | Position -> context.Memory[rawParameter]
        | Immediate -> rawParameter
