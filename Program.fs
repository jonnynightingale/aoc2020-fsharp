module Program

open System.IO

type Problem = {
    dayNumber : int
    filename : string
    solve : string array -> int * int
}

let problems = [
    { dayNumber = 1; filename = @"../../../input/day01.txt"; solve = Day01.Solve }
    { dayNumber = 2; filename = @"../../../input/day02.txt"; solve = Day02.Solve }
]

let PrintSolution ((dayNumber : int), (solution : int * int)) =
    printfn "Day %i" dayNumber
    printfn "  Part one: %i" <| fst solution
    printfn "  Part two: %i" <| snd solution

let Solve problem = File.ReadAllLines problem.filename |> problem.solve

[<EntryPoint>]
let main argv =
    problems
    |> List.map (fun problem -> problem.dayNumber, Solve problem)
    |> List.map PrintSolution
    |> ignore
    0
