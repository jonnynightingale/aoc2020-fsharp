module Day06 =

    // Divides a list, l, into chunks where each chunk contains elements for which
    // the predicate holds
    let divide predicate l =
        let rec impl chunks chunk l =
            match chunk, l with
            | [], [] -> chunks
            | chunk, [] -> chunk::chunks
            | chunk, h::t when predicate h -> impl chunks (h::chunk) t
            | [], h::t -> impl chunks [] t
            | chunk, h::t -> impl (chunk::chunks) [] t
        l |> List.rev |> impl [] []

    let addQuestion questions letter =
        let flagIndex = (int letter) - (int 'a')
        questions ||| (1u <<< flagIndex)

    let parse (input : string array) =
        let parseIndividual (personInput : string) = personInput.ToCharArray() |> Array.fold addQuestion 0u
        let parseGroup (groupInput : string list) = groupInput |> List.map parseIndividual
        input |> Array.toList |> divide (fun s -> s.Length > 0) |> List.map parseGroup

    let countBits (n : uint32) =
        let bitIsSet i = (n &&& (1u <<< i)) > 0u
        [|0..31|] |> Array.filter bitIsSet |> Array.length

    let solve (input : string array) =
        let parsedInput = input |> parse
        let partOneFold = List.fold (|||) 0u
        let partTwoFold = List.fold (&&&) ~~~0u
        let solveWithFold fold = parsedInput |> List.sumBy (fold >> countBits)
        let partOne = solveWithFold partOneFold
        let partTwo = solveWithFold partTwoFold
        string partOne, string partTwo
