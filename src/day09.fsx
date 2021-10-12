module Day09 =

    let groupValues span numbers =
        numbers
        |> Array.skip span
        |> Array.mapi (fun i n -> n, numbers.[i..(i+span-1)])

    let doesNotContainSumPair n ns =
        ns
        |> Array.map (fun k -> n - k)
        |> Array.filter (fun k -> (k + k) <> n)
        |> Array.exists (fun k -> ns |> Array.contains k)
        |> not

    let firstInvalidNumber =
        Array.filter (fun (a, b) -> doesNotContainSumPair a b)
        >> Array.head
        >> fst

    let firstSliceThatSumsTo target (numbers : int64 array) =
        let rec impl min max =
            let window = numbers.[min..max]
            match window |> Array.sum with
            | n when n < target -> impl min (max+1)
            | n when n > target -> impl (min+1) max
            | _ -> window
        impl 0 1

    let sumOfMinAndMax numbers = (+) (numbers |> Array.min) (numbers |> Array.max)

    let solve (input : string array) =
        let numbers = input |> Array.map int64
        let partOne = numbers |> groupValues 25 |> firstInvalidNumber
        let partTwo = numbers |> firstSliceThatSumsTo partOne |> sumOfMinAndMax
        uint64 partOne, uint64 partTwo

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day09.solve
printfn "Day 09: [ %i, %i ]" (fst solution) (snd solution)
