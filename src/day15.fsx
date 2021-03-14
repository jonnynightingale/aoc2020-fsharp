module Day15 =

    open System.Collections.Generic

    let private NthValue2 n (numbers : int array) =
        let lastSeenIndex = new Dictionary<int, int> ()
        let rec Impl (index : int) (num : int) =
            if index = n then num
            else
                let nextIndex = index + 1
                let nextNum =
                    match lastSeenIndex.TryGetValue num with
                    | true, value -> index - value
                    | false, _ -> 0
                lastSeenIndex.[num] <- index
                Impl nextIndex nextNum
        numbers |> Array.indexed |> Array.iter (fun (a, b) -> lastSeenIndex.Add (b, a + 1))
        Impl (numbers.Length + 1) 0

    let Solve (input : string array) =
        let numbers = input.[0].Split ',' |> Array.map int
        let partOneSolution = numbers |> NthValue2 2020
        let partTwoSolution = numbers |> NthValue2 30000000
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day15.Solve
printfn "Day 15: [ %i, %i ]" (fst solution) (snd solution)
