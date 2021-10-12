#load "solvers.fsx"

open Solvers

let printSolution dayNumber inputFile =
    let solver = getSolver dayNumber
    let solution = inputFile |> System.IO.File.ReadAllLines |> solver
    solution ||> printfn "Day %02i: [ %s, %s ]" dayNumber

let dayNumbers = fsi.CommandLineArgs.[1].Split ',' |> Array.map int
let inputFiles = fsi.CommandLineArgs.[2].Split ','
(dayNumbers, inputFiles) ||> Array.iter2 printSolution
