namespace AOC2019.Solutions

open System.Collections.Generic
open Shared.IntcodeComputer.Processor
open Shared.IntcodeComputer.Handlers

module Day05 =
    let public solvePart01 (input: string) =
        let intCode = input.Split "," |> Array.map int
        let processor = Processor intCode
        processor.RegisterHandler 1 additionHandler
        processor.RegisterHandler 2 multiplyHandler
        processor.RegisterHandler 99 haltHandler
        processor.RegisterHandler 3 inputHandler
        processor.RegisterHandler 4 outputHandler

        processor.Execute(Queue [ 1 ])

    let solvePart02 (input: string) =
        let intCode = input.Split "," |> Array.map int
        let processor = Processor intCode
        processor.RegisterHandler 1 additionHandler
        processor.RegisterHandler 2 multiplyHandler
        processor.RegisterHandler 99 haltHandler
        processor.RegisterHandler 3 inputHandler
        processor.RegisterHandler 4 outputHandler
        processor.RegisterHandler 5 jumpIfTrueHandler
        processor.RegisterHandler 6 jumpIfFalseHandler
        processor.RegisterHandler 7 lessThanHandler
        processor.RegisterHandler 8 equalToHandler

        processor.Execute(Queue [ 5 ])
