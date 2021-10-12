module Day01 =

    /// Searches for the first pair of numbers in a collection that sum to a given
    /// target. If found, this returns an option containing the product of those two
    /// numbers.
    let rec productPair target numbers =
        match numbers with
        | n::ns ->
            let m = target - n
            if List.contains m ns
            then Some (n * m)
            else productPair target ns
        | [] -> None

    /// Searches for the first triplet of numbers in a collection that sum to a
    /// given target. If found, this returns an option containing the product of
    /// those three numbers.
    let rec productTriplet target numbers =
        match numbers with
        | n::ns ->
            match productPair (target - n) ns with
            | Some (pairProduct) -> Some (n * pairProduct)
            | None -> productTriplet target ns
        | [] -> None

    let solve (input : string array) =
        let numbers = input |> Array.map int |> Array.toList
        let partOne = productPair 2020 numbers |> Option.get
        let partTwo = productTriplet 2020 numbers |> Option.get
        string partOne, string partTwo
