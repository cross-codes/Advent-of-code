namespace AOC2019.Solutions.Shared.IntcodeComputer

open Instructions
open Contexts

module Handlers =
    type public Handler = Context -> Instruction -> bool // true to keep running, false else

    let public additionHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let addr = context.Memory[context.Pointer + 3]
            let a = interpretParameter context instruction.Modes[0] p1
            let b = interpretParameter context instruction.Modes[1] p2
            context.Memory[addr] <- a + b
            context.Pointer <- context.Pointer + 4
            true

    let public multiplyHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let addr = context.Memory[context.Pointer + 3]
            let a = interpretParameter context instruction.Modes[0] p1
            let b = interpretParameter context instruction.Modes[1] p2
            context.Memory[addr] <- a * b
            context.Pointer <- context.Pointer + 4
            true

    let public haltHandler: Handler = fun _ctx _instr -> false

    let public inputHandler: Handler =
        fun context _instruction ->
            let addr = context.Memory[context.Pointer + 1]

            match context.FixedInput with
            | Some e -> context.Memory[addr] <- e
            | None -> context.Memory[addr] <- stdin.ReadLine() |> int

            context.Pointer <- context.Pointer + 2
            true

    let public outputHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let result = interpretParameter context instruction.Modes[0] p1
            printfn $"[INTCODE CONSOLE]: {result}"
            context.Pointer <- context.Pointer + 2
            true

    let public jumpIfTrueHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let setPointer = interpretParameter context instruction.Modes[0] p1

            if setPointer <> 0 then
                let p2 = context.Memory[context.Pointer + 2]
                let newPosition = interpretParameter context instruction.Modes[1] p2
                context.Pointer <- newPosition
            else
                context.Pointer <- context.Pointer + 3

            true

    let public jumpIfFalseHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let setPointer = interpretParameter context instruction.Modes[0] p1

            if setPointer = 0 then
                let p2 = context.Memory[context.Pointer + 2]
                let newPosition = interpretParameter context instruction.Modes[1] p2
                context.Pointer <- newPosition
            else
                context.Pointer <- context.Pointer + 3

            true

    let public lessThanHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let addr = context.Memory[context.Pointer + 3]
            let val1 = interpretParameter context instruction.Modes[0] p1
            let val2 = interpretParameter context instruction.Modes[1] p2

            if val1 < val2 then
                context.Memory[addr] <- 1
            else
                context.Memory[addr] <- 0

            context.Pointer <- context.Pointer + 4
            true

    let public equalToHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let addr = context.Memory[context.Pointer + 3]
            let val1 = interpretParameter context instruction.Modes[0] p1
            let val2 = interpretParameter context instruction.Modes[1] p2

            if val1 = val2 then
                context.Memory[addr] <- 1
            else
                context.Memory[addr] <- 0

            context.Pointer <- context.Pointer + 4
            true
