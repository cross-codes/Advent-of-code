namespace AOC2019.Solutions.Shared.IntcodeComputer

open ParameterMode

module Instructions =
    type public Instruction = { Opcode: int; Modes: ParameterMode array }

    let public decode (opcode: int) : Instruction option =
        let modeDigit (n: int) = opcode / pown 10 (n + 2) % 10
        let C = toMode (modeDigit 0)
        let B = toMode (modeDigit 1)
        let A = toMode (modeDigit 2)

        match C, B, A with
        | Some c, Some b, Some a ->
            Some
                { Opcode = opcode % 100
                  Modes = [| c; b; a |] }
        | _ -> None
