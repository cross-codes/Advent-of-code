namespace AOC2019.Solutions.Shared.IntcodeComputer

open ParameterMode

module Instructions =
    type public Instruction =
        { Opcode: int
          Modes: ParameterMode array }

    let public decode (opcode: int64) : Instruction option =
        let modeDigit (n: int) = int (opcode / pown 10L (n + 2) % 10L)
        let C = toMode (modeDigit 0)
        let B = toMode (modeDigit 1)
        let A = toMode (modeDigit 2)

        match C, B, A with
        | Some c, Some b, Some a ->
            Some
                { Opcode = int (opcode % 100L)
                  Modes = [| c; b; a |] }
        | _ -> None
