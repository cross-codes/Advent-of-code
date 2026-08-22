namespace AOC2019.Utils

open System.IO

module FileReader =
    let readLines (day: int) =
        let path = sprintf "Inputs/day%02i.txt" day
        File.ReadAllLines path

    let readText (day: int) =
        let path = sprintf "Inputs/day%02i.txt" day
        File.ReadAllText path
