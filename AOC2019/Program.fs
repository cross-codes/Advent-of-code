open AOC2019.Utils
open AOC2019.Solutions

[<EntryPoint>]
let main args =
    match args with
    | [| "day01" |] ->
        let input = FileReader.readLines 1
        input |> Day01.solvePart01 |> printfn "Part 1: %A"
        input |> Day01.solvePart02 |> printfn "Part 2: %A"
        0

    | [| "day02" |] ->
        let input = FileReader.readText 2
        input |> Day02.solvePart01 |> printfn "Part 1: %A"
        input |> Day02.solvePart02 19690720 |> printfn "Part 2: %A"
        0

    | [| "day03" |] ->
        let input = FileReader.readLines 3
        input |> Day03.solvePart01 |> printfn "Part 1: %A"
        input |> Day03.solvePart02 |> printfn "Part 2: %A"
        0

    | _ ->
        printfn "Usage: dotnet run -- day<number>"
        1
