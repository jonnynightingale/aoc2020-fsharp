module Day03

type private Slope = { x : int; y : int }

let private partOneSlope = { x = 3; y = 1 }

let private partTwoSlopes = [|
    { x = 1; y = 1 }
    { x = 3; y = 1 }
    { x = 5; y = 1 }
    { x = 7; y = 1 }
    { x = 1; y = 2 }
|]

let private NumberOfTreesOnRoute (input : string array) mapWidth slope =
    input
    |> Array.indexed
    |> Array.filter (fun (i, _) -> (i % slope.y) = 0)
    |> Array.mapi (fun i (_, row) -> (i * slope.x) % mapWidth, row)
    |> Array.filter (fun (i, row) -> row.Chars i = '#')
    |> Array.length

let private PartOneSolution input mapWidth =
    NumberOfTreesOnRoute input mapWidth partOneSlope |> uint

let private PartTwoSolution input mapWidth =
    partTwoSlopes
    |> Array.map (NumberOfTreesOnRoute input mapWidth)
    |> Array.map uint
    |> Array.reduce (*)

let Solve (input : string array) =
    let mapWidth = input.[0].Length
    let partOneSolution = PartOneSolution input mapWidth
    let partTwoSolution = PartTwoSolution input mapWidth
    partOneSolution, partTwoSolution
