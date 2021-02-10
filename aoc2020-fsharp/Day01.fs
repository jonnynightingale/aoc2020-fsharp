module Day01

/// Searches for the first pair of numbers in a collection that sum to a given
/// target. If found, this returns an option containing the product of those two
/// numbers.
let rec private ProductPair target numbers =
    match numbers with
    | n::ns ->
        let m = target - n
        if List.contains m ns
        then Some(n * m)
        else ProductPair target ns
    | [] -> None

/// Searches for the first triplet of numbers in a collection that sum to a
/// given target. If found, this returns an option containing the product of
/// those three numbers.
let rec private ProductTriplet target numbers =
    match numbers with
    | n::ns ->
        match ProductPair (target - n) ns with
        | Some(pairProduct) -> Some(n * pairProduct)
        | None -> ProductTriplet target ns
    | [] -> None

let Solve (input : string array) =
    let numbers = input |> Array.map int |> Array.toList
    let partOneSolution = ProductPair    2020 numbers |> Option.get
    let partTwoSolution = ProductTriplet 2020 numbers |> Option.get
    uint64 partOneSolution, uint64 partTwoSolution
