namespace AOC2019.Solutions.Shared.IntcodeComputer

module ParameterMode =
    type public ParameterMode =
        | Position
        | Immediate

    let public toMode (digit: int) =
        match digit with
        | 0 -> Some Position
        | 1 -> Some Immediate
        | _ -> None
