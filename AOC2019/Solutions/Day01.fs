namespace AOC2019.Solutions

module Day01 =
    let private getFuelMass mass = mass / 3 - 2

    let public solvePart01 (input: string array) =
        input |> Array.sumBy (int >> getFuelMass)

    let private getModuleFuel mass =
        let rec loop acc currentMass =
            let fuel = getFuelMass currentMass
            if fuel <= 0 then acc else loop (acc + fuel) fuel

        loop 0 mass

    let public solvePart02 (input: string array) =
        input |> Array.sumBy (int >> getModuleFuel)
