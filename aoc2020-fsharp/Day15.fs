module Day15

let private Iterate (numbers : int array) =
    let lastNumber = numbers |> Array.last
    let previousNumbers = numbers.[..(numbers.Length - 2)]
    match previousNumbers |> Array.tryFindIndexBack ((=) lastNumber) with
    | Some(i) -> [| previousNumbers.Length - i |] |> Array.append numbers
    | None -> [| 0 |] |> Array.append numbers

let private IterateN n (numbers : int array) =
    let rec Impl (innerNumbers : int array) =
        if innerNumbers.Length = n then innerNumbers else innerNumbers |> Iterate |> Impl
    Impl numbers

let Solve (input : string array) =
    let numbers = input.[0].Split ',' |> Array.map int
    let partOneSolution = numbers |> IterateN 2020 |> Array.last
    let partTwoSolution = 0
    uint64 partOneSolution, uint64 partTwoSolution
