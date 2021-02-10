module Day09

let GroupValues span numbers =
    numbers
    |> Array.skip span
    |> Array.mapi (fun i n -> n, numbers.[i..(i+span-1)])

let DoesNotContainSumPair n ns =
    ns
    |> Array.map (fun k -> n - k)
    |> Array.filter (fun k -> (k + k) <> n)
    |> Array.filter (fun k -> ns |> Array.contains k)
    |> Array.isEmpty

let FirstInvalidNumber =
    Array.filter (fun (a, b) -> DoesNotContainSumPair a b)
    >> Array.head
    >> fst

let Solve (input : string array) =
    let numbers = input |> Array.map int64
    let partOneSolution = numbers |> GroupValues 25 |> FirstInvalidNumber
    uint partOneSolution, 0u
