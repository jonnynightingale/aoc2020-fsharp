module Day15 =

    open System.Collections.Generic

    let nthValue2 n (numbers : int array) =
        let lastSeenIndex = new Dictionary<int, int> ()
        let rec impl (index : int) (num : int) =
            if index = n then num
            else
                let nextNum =
                    match lastSeenIndex.TryGetValue num with
                    | true, value -> index - value
                    | false, _ -> 0
                lastSeenIndex.[num] <- index
                impl (index + 1) nextNum
        numbers |> Array.indexed |> Array.iter (fun (a, b) -> lastSeenIndex.Add (b, a + 1))
        impl (numbers.Length + 1) 0

    let solve (input : string array) =
        let numbers = input.[0].Split ',' |> Array.map int
        let partOne = numbers |> nthValue2 2020
        let partTwo = numbers |> nthValue2 30000000
        uint64 partOne, uint64 partTwo

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day15.solve
printfn "Day 15: [ %i, %i ]" (fst solution) (snd solution)
