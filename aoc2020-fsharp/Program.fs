module Program

open System.IO

type Problem = {
    dayNumber : int
    filename : string
    solve : string array -> uint * uint
}

let problems = [
    { dayNumber = 1; filename = @"../../../../input/day01.txt"; solve = Day01.Solve }
    { dayNumber = 2; filename = @"../../../../input/day02.txt"; solve = Day02.Solve }
    { dayNumber = 3; filename = @"../../../../input/day03.txt"; solve = Day03.Solve }
    { dayNumber = 4; filename = @"../../../../input/day04.txt"; solve = Day04.Solve }
    { dayNumber = 5; filename = @"../../../../input/day05.txt"; solve = Day05.Solve }
    { dayNumber = 6; filename = @"../../../../input/day06.txt"; solve = Day06.Solve }
    { dayNumber = 7; filename = @"../../../../input/day07.txt"; solve = Day07.Solve }
    { dayNumber = 8; filename = @"../../../../input/day08.txt"; solve = Day08.Solve }
    { dayNumber = 9; filename = @"../../../../input/day09.txt"; solve = Day09.Solve }
]

let PrintSolution (dayNumber, solution) =
    printfn "Day %i" dayNumber
    printfn "  Part one: %i" <| fst solution
    printfn "  Part two: %i" <| snd solution

let Solve problem = File.ReadAllLines problem.filename |> problem.solve

[<EntryPoint>]
let main _ =
    problems
    |> List.map (fun problem -> problem.dayNumber, Solve problem)
    |> List.iter PrintSolution
    0
