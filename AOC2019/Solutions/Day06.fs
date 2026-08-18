namespace AOC2019.Solutions

open System.Collections.Generic

module Day06 =
    let private dfs (neighbors: string -> string list) (start: string) : seq<string> =
        let rec loop visited stack =
            seq {
                match stack with
                | [] -> ()
                | curr :: rest ->
                    if not (Set.contains curr visited) then
                        yield curr
                        let nbd = neighbors curr
                        yield! loop (Set.add curr visited) (nbd @ rest)
                    else
                        yield! loop visited rest
            }

        loop Set.empty [ start ]

    let private createNeighborsMapping (edges: (string * string) list) : (string -> string list) =
        let lookupMap =
            edges
            |> List.groupBy fst
            |> List.map (fun (src, grp) -> src, grp |> List.map snd)
            |> Map.ofList

        fun node -> lookupMap |> Map.tryFind node |> Option.defaultValue []

    let public solvePart01 (input: string array) =
        let mutable edges = List.empty
        let mutable nodes = Set.empty

        for dscr in input do
            let edge = dscr.Split ')'
            edges <- (edge[1], edge[0]) :: edges
            nodes <- Set.add edge[0] nodes
            nodes <- Set.add edge[1] nodes

        let neighbors = createNeighborsMapping edges

        let mutable count = 0

        for node in nodes do
            let path = node |> dfs neighbors
            count <- count + Seq.length path - 1

        count


    let private shortestPath (neighbors: string -> string list) (start: string) (target: string) : string list option =

        if start = target then
            Some [ start ]
        else

            let queue = Queue<string>()
            let parent = Dictionary<string, string>()

            queue.Enqueue start
            parent[start] <- start

            let mutable found = false

            while queue.Count > 0 && not found do
                let curr = queue.Dequeue()

                if curr = target then
                    found <- true
                else
                    for nbd in neighbors curr do
                        if not (parent.ContainsKey nbd) then
                            parent[nbd] <- curr
                            queue.Enqueue nbd

            if found then
                let rec reconstruct curr acc =
                    let p = parent[curr]

                    if p = curr then
                        curr :: acc
                    else
                        reconstruct p (curr :: acc)

                Some(reconstruct target [])
            else
                None

    let public solvePart02 (input: string array) =
        let mutable edges = List.empty

        for dscr in input do
            let edge = dscr.Split ')'
            edges <- (edge[1], edge[0]) :: edges
            edges <- (edge[0], edge[1]) :: edges

        let neighbors = createNeighborsMapping edges

        match shortestPath neighbors "YOU" "SAN" with
        | Some path -> path.Length - 3
        | None -> -1
