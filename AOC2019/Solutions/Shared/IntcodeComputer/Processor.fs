namespace AOC2019.Solutions.Shared.IntcodeComputer

open Instructions
open Contexts
open Handlers
open System.Collections.Generic

module Processor =

    type public Processor(initialOpcodes: int array) =
        let memory = Array.copy initialOpcodes
        let mutable handlers: Map<int, Handler> = Map.empty

        member _.RegisterHandler opcode handler =
            handlers <- handlers |> Map.add opcode handler

        member _.ModifyAt index value = memory.[index] <- value

        member this.Run(input: Queue<int>) : Result<(int list * int array), string> =
            let context =
                { Memory = memory
                  Pointer = 0
                  InputQueue = input
                  Outputs = List<int>() }

            let rec executeNext () =
                match decode memory[context.Pointer] with
                | Some instruction ->
                    match handlers.TryFind instruction.Opcode with
                    | Some handler ->
                        let continueInterpretation = handler context instruction
                        if continueInterpretation then
                            executeNext ()
                        else
                            Ok (context.Outputs |> List.ofSeq, context.Memory)
                    | None ->
                        Error $"No handler registered for opcode {instruction.Opcode} at pointer {context.Pointer}"
                | None ->
                    Error $"Failed to decode opcode {memory[context.Pointer]} at pointer {context.Pointer}"

            executeNext ()
