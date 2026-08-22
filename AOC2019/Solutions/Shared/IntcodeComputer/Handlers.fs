namespace AOC2019.Solutions.Shared.IntcodeComputer

open Instructions
open Contexts

module Handlers =
    type public HandlerResult =
        | Continue
        | OutputProduced
        | Halted

    type public Handler = Context -> Instruction -> HandlerResult

    let public additionHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let a = interpretParameter context instruction.Modes[0] p1
            let b = interpretParameter context instruction.Modes[1] p2
            let addr = getWriteAddress context instruction.Modes[2] context.Memory[context.Pointer + 3]
            context.Memory[int addr] <- a + b
            context.Pointer <- context.Pointer + 4
            Continue

    let public multiplyHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let a = interpretParameter context instruction.Modes[0] p1
            let b = interpretParameter context instruction.Modes[1] p2
            let addr = getWriteAddress context instruction.Modes[2] context.Memory[context.Pointer + 3]
            context.Memory[int addr] <- a * b
            context.Pointer <- context.Pointer + 4
            Continue

    let public haltHandler: Handler = fun _ctx _instr -> Halted

    let public inputHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let addr = getWriteAddress context instruction.Modes[0] p1

            if context.InputQueue.Count = 0 then
                failwith $"No input provided"
            else
                context.Memory[int addr] <- context.InputQueue.Dequeue()

            context.Pointer <- context.Pointer + 2
            Continue

    let public outputHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let result = interpretParameter context instruction.Modes[0] p1
            context.Outputs.Add result
            context.Pointer <- context.Pointer + 2
            OutputProduced

    let public jumpIfTrueHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let setPointer = interpretParameter context instruction.Modes[0] p1

            if setPointer <> 0L then
                let p2 = context.Memory[context.Pointer + 2]
                let newPosition = interpretParameter context instruction.Modes[1] p2
                context.Pointer <- int newPosition
            else
                context.Pointer <- context.Pointer + 3

            Continue

    let public jumpIfFalseHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let setPointer = interpretParameter context instruction.Modes[0] p1

            if setPointer = 0 then
                let p2 = context.Memory[context.Pointer + 2]
                let newPosition = interpretParameter context instruction.Modes[1] p2
                context.Pointer <- int newPosition
            else
                context.Pointer <- context.Pointer + 3

            Continue

    let public lessThanHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let val1 = interpretParameter context instruction.Modes[0] p1
            let val2 = interpretParameter context instruction.Modes[1] p2
            let addr = getWriteAddress context instruction.Modes[2] context.Memory[context.Pointer + 3]

            if val1 < val2 then
                context.Memory[int addr] <- 1
            else
                context.Memory[int addr] <- 0

            context.Pointer <- context.Pointer + 4
            Continue

    let public equalToHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let p2 = context.Memory[context.Pointer + 2]
            let val1 = interpretParameter context instruction.Modes[0] p1
            let val2 = interpretParameter context instruction.Modes[1] p2
            let addr = getWriteAddress context instruction.Modes[2] context.Memory[context.Pointer + 3]

            if val1 = val2 then
                context.Memory[int addr] <- 1
            else
                context.Memory[int addr] <- 0

            context.Pointer <- context.Pointer + 4
            Continue

    let public relativeOffSetIncrementHandler: Handler =
        fun context instruction ->
            let p1 = context.Memory[context.Pointer + 1]
            let val1 = interpretParameter context instruction.Modes[0] p1
            context.CurrentRelativeBase <- context.CurrentRelativeBase + val1
            context.Pointer <- context.Pointer + 2
            Continue
