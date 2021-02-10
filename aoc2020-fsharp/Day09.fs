module Day09

let private GroupValues span numbers =
    numbers
    |> Array.skip span
    |> Array.mapi (fun i n -> n, numbers.[i..(i+span-1)])

let private DoesNotContainSumPair n ns =
    ns
    |> Array.map (fun k -> n - k)
    |> Array.filter (fun k -> (k + k) <> n)
    |> Array.filter (fun k -> ns |> Array.contains k)
    |> Array.isEmpty

let private FirstInvalidNumber =
    Array.filter (fun (a, b) -> DoesNotContainSumPair a b)
    >> Array.head
    >> fst

let private FirstSliceThatSumsTo target (numbers : int64 array) =
    let rec Impl min max =
        let window = numbers.[min..max]
        match window |> Array.sum with
        | n when n < target -> Impl min (max+1)
        | n when n > target -> Impl (min+1) max
        | _ -> window
    Impl 0 1

let private SumOfMinAndMax (numbers : int64 array) =
    (+) (numbers |> Array.min) (numbers |> Array.max)

let Solve (input : string array) =
    let numbers = input |> Array.map int64
    let partOneSolution = numbers |> GroupValues 25 |> FirstInvalidNumber
    let partTwoSolution = numbers |> FirstSliceThatSumsTo partOneSolution |> SumOfMinAndMax
    uint partOneSolution, uint partTwoSolution
