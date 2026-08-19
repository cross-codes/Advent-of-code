namespace AOC2019.Solutions.Shared.IntcodeComputer

open System.Collections.Generic
open ParameterMode

module Contexts =
    type public Context =
        { Memory: int array
          mutable Pointer: int
          InputQueue: Queue<int>
          Outputs: List<int> }

    let public interpretParameter context mode rawParameter =
        match mode with
        | Position -> context.Memory[rawParameter]
        | Immediate -> rawParameter
