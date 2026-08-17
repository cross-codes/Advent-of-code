namespace AOC2019.Solutions.Shared.IntcodeComputer

open Instructions
open Contexts
open Handlers

module Processor =

    type public Processor(initialOpcodes: int array) =
        let memory = Array.copy initialOpcodes
        let mutable handlers: Map<int, Handler> = Map.empty

        member _.RegisterHandler opcode handler =
            handlers <- handlers |> Map.add opcode handler

        member _.ModifyAt index value = memory.[index] <- value

        member this.Run?fixedInput =
            let context =
                { Memory = memory
                  Pointer = 0
                  FixedInput = fixedInput }

            let mutable error = false

            let rec executeNext () =
                match decode memory[context.Pointer] with
                | Some instruction ->
                    match handlers.TryFind instruction.Opcode with
                    | Some handler ->
                        let continueInterpretation = handler context instruction

                        if continueInterpretation then
                            executeNext ()
                    | None ->
                        failwith $"No handler registered for opcode {instruction.Opcode} at pointer {context.Pointer}"
                | None -> error <- true

            executeNext ()

            if not error then
                context.Memory.[0]
            else
                System.Int32.MinValue
