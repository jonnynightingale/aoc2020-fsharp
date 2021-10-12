module Day03 =

    type Slope = { X : int; Y : int }

    let partOneSlope = { X = 3; Y = 1 }

    let partTwoSlopes = [|
        { X = 1; Y = 1 }
        { X = 3; Y = 1 }
        { X = 5; Y = 1 }
        { X = 7; Y = 1 }
        { X = 1; Y = 2 }
    |]

    let numberOfTreesOnRoute (input : string array) mapWidth slope =
        input
        |> Array.indexed
        |> Array.filter (fun (i, _) -> (i % slope.Y) = 0)
        |> Array.mapi (fun i (_, row) -> (i * slope.X) % mapWidth, row)
        |> Array.filter (fun (i, row) -> row.Chars i = '#')
        |> Array.length

    let solvePartTwo input mapWidth =
        partTwoSlopes
        |> Array.map (numberOfTreesOnRoute input mapWidth >> uint64)
        |> Array.reduce (*)

    let solve (input : string array) =
        let mapWidth = input.[0].Length
        let partOneSolution = numberOfTreesOnRoute input mapWidth partOneSlope
        let partTwoSolution = solvePartTwo input mapWidth
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day03.solve
printfn "Day 03: [ %i, %i ]" (fst solution) (snd solution)
