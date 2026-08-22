namespace AOC2019.Solutions.Shared.IntcodeComputer

open Instructions
open Contexts
open Handlers
open System.Collections.Generic

module Processor =

    type public ExecutionResult =
        | Paused
        | Terminated

    type public Processor(initialOpcodes: int64 array) =
        let memory = Array.zeroCreate<int64> 1_000_000
        do Array.blit initialOpcodes 0 memory 0 initialOpcodes.Length

        let mutable handlers: Map<int, Handler> = Map.empty

        member _.RegisterHandler opcode handler =
            handlers <- handlers |> Map.add opcode handler

        member _.ModifyAt index value = memory.[index] <- value

        member this.Execute(additionalInput: Queue<int64>, ?existingContext: Context, ?pauseOnOutput: bool) =
            let context =
                match existingContext with
                | Some ctx ->
                    additionalInput |> Seq.iter ctx.InputQueue.Enqueue
                    ctx
                | None ->
                    { Memory = memory
                      Pointer = 0
                      CurrentRelativeBase = 0
                      InputQueue = additionalInput
                      Outputs = List<int64>() }

            let pauseOnOutput = defaultArg pauseOnOutput false

            let rec executeNext () =
                match decode memory[context.Pointer] with
                | Some instruction ->
                    match handlers.TryFind instruction.Opcode with
                    | Some handler ->
                        let instructionAftermath = handler context instruction

                        match instructionAftermath with
                        | Continue -> executeNext ()
                        | OutputProduced when pauseOnOutput ->
                            Ok(context.Outputs |> List.ofSeq, context.Memory, context, Paused)
                        | OutputProduced -> executeNext ()
                        | Halted -> Ok(context.Outputs |> List.ofSeq, context.Memory, context, Terminated)

                    | None ->
                        Error $"No handler registered for opcode {instruction.Opcode} at pointer {context.Pointer}"
                | None -> Error $"Failed to decode opcode {memory[context.Pointer]} at pointer {context.Pointer}"

            executeNext ()
