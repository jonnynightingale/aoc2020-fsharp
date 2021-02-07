module Day06

// Divides a list, l, into chunks where each chunk contains elements for which
// the predicate holds
let private Divide predicate l =
    let rec Impl chunks chunk l =
        match chunk, l with
        | [], [] -> chunks
        | chunk, [] -> chunk::chunks
        | chunk, h::t when predicate h -> Impl chunks (h::chunk) t
        | [], h::t -> Impl chunks [] t
        | chunk, h::t -> Impl (chunk::chunks) [] t
    l |> List.rev |> Impl [] []

let private AddQuestion questions letter =
    let flagIndex = (int letter) - (int 'a')
    questions ||| (1u <<< flagIndex)

let private Parse (input : string array) =
    let ParseIndividual (personInput : string) = personInput.ToCharArray() |> Array.fold AddQuestion 0u
    let ParseGroup (groupInput : string list) = groupInput |> List.map ParseIndividual
    input |> Array.toList |> Divide (fun s -> s.Length > 0) |> List.map ParseGroup

let CountBits (n : uint32) =
    let BitIsSet i = (n &&& (1u <<< i)) > 0u
    [|0..31|] |> Array.filter BitIsSet |> Array.length

let Solve (input : string array) =
    let parsedInput = input |> Parse
    let PartOneFold = List.fold (|||) 0u
    let PartTwoFold = List.fold (&&&) ~~~0u
    let SolveWithFold fold =
        parsedInput |> List.map fold |> List.map CountBits |> List.sum
    let partOneSolution = SolveWithFold PartOneFold
    let partTwoSolution = SolveWithFold PartTwoFold
    uint partOneSolution, uint partTwoSolution
