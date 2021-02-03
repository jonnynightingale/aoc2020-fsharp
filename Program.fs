module Program

open System.IO

type Problem = {
    dayNumber : int
    filename : string
    solve : string array -> int * int
}

let problems = [
    { dayNumber = 1; filename = @"../../../input/day01.txt"; solve = Day01.solve }
]

let PrintSolution (dayNumber : int) (solution : int * int) =
    printfn "Day %i" dayNumber
    fst solution |> printfn "  Part one: %i"
    snd solution |> printfn "  Part two: %i"

let Solve problem = File.ReadAllLines problem.filename |> problem.solve

[<EntryPoint>]
let main argv =
    problems
    |> List.map (fun problem -> problem.dayNumber, Solve problem)
    |> List.map (fun x -> PrintSolution (fst x) (snd x))
    |> ignore
    0
