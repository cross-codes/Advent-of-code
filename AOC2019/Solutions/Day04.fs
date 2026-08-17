namespace AOC2019.Solutions

module Day04 =
    let public solvePart01 (input: string) =
        let range = input.Split "-"
        let rStart = range[0] |> int
        let rEnd = range[1] |> int

        let isValid (num: string) =
            let pairs = Seq.pairwise num

            Seq.forall (fun (a, b) -> a <= b) pairs
            && Seq.exists (fun (a, b) -> a = b) pairs

        let rec testRange curr acc =
            if curr > rEnd then
                acc
            else if isValid (curr.ToString()) then
                testRange (curr + 1) (acc + 1)
            else
                testRange (curr + 1) acc

        testRange rStart 0

    let public solvePart02 (input: string) =
        let range = input.Split "-"
        let rStart = range[0] |> int
        let rEnd = range[1] |> int

        let isValid (num: string) =
            let padded = $" {num} "
            let pairs = Seq.pairwise num

            Seq.forall (fun (a, b) -> a <= b) pairs
            && padded
               |> Seq.windowed 4
               |> Seq.exists (function
                   | [| prev; a; b; next |] -> a = b && a <> prev && b <> next
                   | _ -> false)

        let rec testRange curr acc =
            if curr > rEnd then
                acc
            else if isValid (curr.ToString()) then
                testRange (curr + 1) (acc + 1)
            else
                testRange (curr + 1) acc

        testRange rStart 0
