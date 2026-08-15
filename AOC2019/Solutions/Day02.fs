namespace AOC2019.Solutions

module Day02 =
    let private getOutput (intCode: int array) rep1 rep2 =
        let copy = Array.copy intCode
        copy[1] <- rep1
        copy[2] <- rep2

        let rec operate i =
            match copy[i] with
            | 1 ->
                let a = copy[copy[i + 1]]
                let b = copy[copy[i + 2]]
                let targetPos = copy[i + 3]
                copy[targetPos] <- a + b
                operate (i + 4)
            | 2 ->
                let a = copy[copy[i + 1]]
                let b = copy[copy[i + 2]]
                let targetPos = copy[i + 3]
                copy[targetPos] <- a * b
                operate (i + 4)
            | 99 -> Some(copy[0])
            | _ -> None

        let res = operate 0

        res

    let solvePart01 (input: string) =
        let intCode = input.Split "," |> Array.map int
        Option.get (getOutput intCode 12 2)

    let solvePart02 targetOutput (input: string) =
        let intCode = input.Split "," |> Array.map int

        let resPair =
            Seq.allPairs (seq { 0..99 }) (seq { 0..99 })
            |> Seq.tryFind (fun (i, j) ->
                match getOutput intCode i j with
                | Some e -> e = targetOutput
                | None -> false)

        match resPair with
        | Some(i, j) -> 100 * i + j
        | None -> failwith "No matching pair found"
