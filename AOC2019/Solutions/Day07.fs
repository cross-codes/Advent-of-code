namespace AOC2019.Solutions

open Combinatorics.Collections
open System.Collections.Generic
open Shared.IntcodeComputer.Processor
open Shared.IntcodeComputer.Handlers
open Shared.IntcodeComputer.Contexts

module Day07 =
    type private Amplifier = int -> int -> int

    type private StatefulAmplifier =
        { Processor: Processor
          Phase: int
          Context: Context option
          Status: ExecutionResult option }

    let private setupProcessor (processor: Processor) =
        processor.RegisterHandler 1 additionHandler
        processor.RegisterHandler 2 multiplyHandler
        processor.RegisterHandler 99 haltHandler
        processor.RegisterHandler 3 inputHandler
        processor.RegisterHandler 4 outputHandler
        processor.RegisterHandler 5 jumpIfTrueHandler
        processor.RegisterHandler 6 jumpIfFalseHandler
        processor.RegisterHandler 7 lessThanHandler
        processor.RegisterHandler 8 equalToHandler

    module private Amplifier =
        let create (intCode: int array) : Amplifier =
            fun phase signal ->
                let processor = Processor intCode
                setupProcessor processor

                match processor.Execute(Queue [ phase; signal ]) with
                | Ok(outputs, _, _, _) -> List.head outputs
                | Error msg -> failwith msg

        let createStatefulAmplifiers (intCode: int array) (phases: seq<int>) : seq<StatefulAmplifier> =
            phases
            |> Seq.map (fun phase ->
                let processor = Processor intCode
                setupProcessor processor

                { Processor = processor
                  Phase = phase
                  Context = None
                  Status = None })

    let public solvePart01 (input: string) =
        let intCode = input.Split ',' |> Array.map int

        let runChain (phases: seq<int>) =
            phases
            |> Seq.map (Amplifier.create intCode)
            |> Seq.fold (fun acc nextAmp -> nextAmp acc) 0

        Permutations [| 0; 1; 2; 3; 4 |] |> Seq.map runChain |> Seq.max

    let public solvePart02 (input: string) =
        let intCode = input.Split ',' |> Array.map int

        let runFeedbackLoop (phases: seq<int>) =
            let amplifiers = Amplifier.createStatefulAmplifiers intCode phases |> Array.ofSeq

            let runAmplifier (amplifier: StatefulAmplifier) (signal: int) =
                let inputQueue =
                    match amplifier.Context with
                    | None -> Queue [ amplifier.Phase; signal ]
                    | Some _ -> Queue [ signal ]

                amplifier.Processor.Execute(inputQueue, ?existingContext = amplifier.Context, pauseOnOutput = true)

            let rec loop (signal: int) (idx: int) =
                if idx = 0 then
                    let amplifier = amplifiers[idx]

                    match amplifier.Status with
                    | Some Terminated -> signal
                    | _ ->
                        match runAmplifier amplifier signal with
                        | Ok(outputs, _, context, status) ->
                            amplifiers[idx] <-
                                { amplifier with
                                    Context = Some context
                                    Status = Some status }

                            let fwd = List.last outputs
                            loop fwd (1 + idx)
                        | Error msg -> failwith msg
                else
                    let amplifier = amplifiers[idx]

                    match runAmplifier amplifier signal with
                    | Ok(outputs, _, context, status) ->
                        amplifiers[idx] <-
                            { amplifier with
                                Context = Some context
                                Status = Some status }

                        let fwd = List.last outputs
                        loop fwd ((1 + idx) % 5)
                    | Error msg -> failwith msg

            loop 0 0

        Permutations [| 5; 6; 7; 8; 9 |] |> Seq.map runFeedbackLoop |> Seq.max
