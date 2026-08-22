namespace AOC2019.Solutions.Shared.IntcodeComputer

open System.Collections.Generic
open ParameterMode

module Contexts =
    type public Context =
        { Memory: int64 array
          mutable Pointer: int
          mutable CurrentRelativeBase: int64
          InputQueue: Queue<int64>
          Outputs: List<int64> }

    let public interpretParameter context mode (rawParameter: int64) =
        match mode with
        | Position -> context.Memory[int rawParameter]
        | Immediate -> rawParameter
        | Relative -> context.Memory[int (context.CurrentRelativeBase + rawParameter)]

    let public getWriteAddress context mode (rawParameter: int64) =
        match mode with
        | Position -> int rawParameter
        | Relative -> int (context.CurrentRelativeBase + rawParameter)
        | Immediate -> failwith "Write parameters cannot be in immediate mode"
