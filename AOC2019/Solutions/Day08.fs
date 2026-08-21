namespace AOC2019.Solutions

open System

module Day08 =
    let private count (nums: int array) e =
        nums |> Array.sumBy (fun x -> if x = e then 1 else 0)

    let private productOf1And2Digits (nums: int array) = count nums 1 * count nums 2

    let public solvePart01 (input: string) =
        input
        |> Seq.filter Char.IsDigit
        |> Seq.toArray
        |> Array.map (Char.GetNumericValue >> int)
        |> Array.chunkBySize (25 * 6)
        |> Array.minBy (fun arr -> count arr 0)
        |> productOf1And2Digits

    let private interpretLayers (layers: int array array) =
        let n = layers[0].Length

        Array.init n (fun i ->
            layers
            |> Seq.map (fun layer -> layer[i])
            |> Seq.tryFind (fun x -> x <> 2)
            |> Option.defaultValue 2)

    let public solvePart02 (input: string) =
        let image =
            input
            |> Seq.filter Char.IsDigit
            |> Seq.toArray
            |> Array.map (Char.GetNumericValue >> int)
            |> Array.chunkBySize (25 * 6)
            |> interpretLayers

        let body =
            image
            |> Array.map (function
                | 1 -> "█"
                | _ -> " ")
            |> Array.chunkBySize 25
            |> Array.map (String.concat " ")
            |> String.concat "\n"

        "\n" + body + "\n"
