namespace AOC2019.Solutions

open System.Collections.Generic
open Shared.IntcodeComputer.Handlers
open Shared.IntcodeComputer.Processor

module Day02 =
    let public solvePart01 (input: string) =
        let intCode = input.Split "," |> Array.map int
        let processor = Processor intCode
        processor.RegisterHandler 1 additionHandler
        processor.RegisterHandler 2 multiplyHandler
        processor.RegisterHandler 99 haltHandler

        processor.ModifyAt 1 12
        processor.ModifyAt 2 2

        processor.Execute(Queue<int>())


    let public solvePart02 (input: string) =
        let intCode = input.Split "," |> Array.map int

        let resPair =
            Seq.allPairs (seq { 0..99 }) (seq { 0..99 })
            |> Seq.tryFind (fun (i, j) ->
                let processor = Processor intCode
                processor.RegisterHandler 1 additionHandler
                processor.RegisterHandler 2 multiplyHandler
                processor.RegisterHandler 99 haltHandler
                processor.ModifyAt 1 i
                processor.ModifyAt 2 j

                match processor.Execute(Queue<int>()) with
                | Ok(_, memory, _, _) -> memory[0] = 19690720
                | Error _ -> false)

        match resPair with
        | Some(i, j) -> 100 * i + j
        | None -> failwith "No matching pair found"
