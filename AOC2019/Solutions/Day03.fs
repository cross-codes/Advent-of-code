namespace AOC2019.Solutions

open System.Collections.Generic

module Day03 =
    let private simulateMovements (movement: string) (y, x) =
        let dist = movement[1..] |> int

        match movement[0] with
        | 'L' -> [ for cx in x - 1 .. -1 .. x - dist -> y, cx ], (y, x - dist)
        | 'R' -> [ for cx in x + 1 .. x + dist -> y, cx ], (y, x + dist)
        | 'U' -> [ for cy in y + 1 .. y + dist -> cy, x ], (y + dist, x)
        | 'D' -> [ for cy in y - 1 .. -1 .. y - dist -> cy, x ], (y - dist, x)
        | _ -> failwith "Invalid direction found"

    let private getIntersectionPoints (firstMovements: string array) (secondMovements: string array) =
        let firstCoordinates = HashSet<int * int>()
        let secondCoordinates = HashSet<int * int>()

        firstMovements
        |> Array.fold
            (fun currentPos mov ->
                let path, endPos = simulateMovements mov currentPos
                firstCoordinates.UnionWith path
                endPos)
            (0, 0)
        |> ignore


        secondMovements
        |> Array.fold
            (fun currentPos mov ->
                let path, endPos = simulateMovements mov currentPos
                secondCoordinates.UnionWith path
                endPos)
            (0, 0)
        |> ignore

        firstCoordinates.IntersectWith secondCoordinates
        firstCoordinates.Remove(0, 0) |> ignore
        firstCoordinates

    let solvePart01 (input: string array) =
        let firstMovements = input[0].Split ","
        let secondMovements = input[1].Split ","
        let intersectionPoints = getIntersectionPoints firstMovements secondMovements

        intersectionPoints |> Seq.map (fun (y, x) -> abs y + abs x) |> Seq.min

    let private getDistanceMap (movements: string array) =
        let distanceMap = Dictionary<int * int, int>()

        movements
        |> Array.fold
            (fun (curPos, steps) mov ->
                let path, endPos = simulateMovements mov curPos

                let finalSteps =
                    path
                    |> List.fold
                        (fun curStep pos ->
                            let nxtStep = 1 + curStep
                            distanceMap.TryAdd(pos, nxtStep) |> ignore
                            nxtStep)
                        steps

                endPos, finalSteps)
            ((0, 0), 0)
        |> ignore

        distanceMap

    let solvePart02 (input: string array) =
        let firstMovements = input[0].Split ","
        let firstDistanceMap = getDistanceMap firstMovements

        let secondMovements = input[1].Split ","
        let secondDistanceMap = getDistanceMap secondMovements

        let intersectionPoints = getIntersectionPoints firstMovements secondMovements

        intersectionPoints
        |> Seq.map (fun (y, x) -> firstDistanceMap[(y, x)] + secondDistanceMap[(y, x)])
        |> Seq.min
