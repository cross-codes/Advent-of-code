open AOC2019.Utils
open AOC2019.Solutions
open System.Diagnostics

let time (label: string) (f: unit -> unit) =
    let sw = Stopwatch.StartNew()
    f ()
    sw.Stop()
    printfn $"{label} took {sw.ElapsedMilliseconds}ms"

let day01 () =
    let input = FileReader.readLines 1
    input |> Day01.solvePart01 |> printfn "Part 1: %A"
    input |> Day01.solvePart02 |> printfn "Part 2: %A"

let day02 () =
    let input = FileReader.readText 2
    input |> Day02.solvePart01 |> printfn "Part 1: %A"
    input |> Day02.solvePart02 |> printfn "Part 2: %A"

let day03 () =
    let input = FileReader.readLines 3
    input |> Day03.solvePart01 |> printfn "Part 1: %A"
    input |> Day03.solvePart02 |> printfn "Part 2: %A"

let day04 () =
    let input = FileReader.readText 4
    input |> Day04.solvePart01 |> printfn "Part 1: %A"
    input |> Day04.solvePart02 |> printfn "Part 2: %A"

let day05 () =
    let input = FileReader.readText 5
    input |> Day05.solvePart01 |> printfn "Part 1: %A"
    input |> Day05.solvePart02 |> printfn "Part 2: %A"

let day06 () =
    let input = FileReader.readLines 6
    input |> Day06.solvePart01 |> printfn "Part 1: %A"
    input |> Day06.solvePart02 |> printfn "Part 2: %A"

let days =
    [ "day01", day01
      "day02", day02
      "day03", day03
      "day04", day04
      "day05", day05
      "day06", day06 ]

[<EntryPoint>]
let main args =
    match args with
    | [| dayArg |] when dayArg = "all" ->
        for name, f in days do
            time name f

        0

    | [| dayArg |] ->
        match days |> List.tryFind (fun (name, _) -> name = dayArg) with
        | Some(name, f) -> time name f
        | None -> printfn "Unknown day: %s" dayArg

        0

    | _ ->
        printfn "Usage: dotnet run -- day<number> | all"
        1
