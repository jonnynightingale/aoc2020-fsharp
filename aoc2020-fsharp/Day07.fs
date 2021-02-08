module Day07

open System.Text.RegularExpressions

let private ParseLine line =
    let parentMatch = Regex.Match (line, "^(.+?) bags")
    let parent = parentMatch.Groups.[1].Value
    let childrenMatches = Regex.Matches (line, "(\d+) (.+?) bag")
    let children =
        childrenMatches
        |> Seq.cast
        |> Seq.map (fun (regMatch : Match) -> int regMatch.Groups.[1].Value, regMatch.Groups.[2].Value)
        |> Seq.toArray
    parent, children

let private Parse = Array.map ParseLine >> Map.ofArray

let private NumberOfOutermostBagPossibilities bagType lookup =
    let rec BagsThatCanHold bagType =
        let bagsThatCanHoldThis =
            lookup
            |> Map.filter (fun _ value ->
                let n = value |> Array.filter (fun (_, bag) -> bag = bagType) |> Array.length
                n > 0)
            |> Map.toList
            |> List.map fst
        match bagsThatCanHoldThis.Length with
        | 0 -> [ bagType ]
        | _ ->
            bagsThatCanHoldThis
            |> List.fold (fun acc b -> b |> BagsThatCanHold |> List.append acc) [ bagType ]
    bagType |> BagsThatCanHold |> List.distinct |> List.length |> (+) -1

let private NumberOfBagsInside bagType (lookup : Map<string, (int * string)[]>) =
    let rec NumberOfBagsInsideImpl bagType =
        lookup.[bagType] |> Array.sumBy (fun (n, b) -> n + (n * NumberOfBagsInsideImpl b))
    NumberOfBagsInsideImpl bagType

let Solve (input : string array) =
    let parsedInput = input |> Parse
    let partOneSolution = parsedInput |> NumberOfOutermostBagPossibilities "shiny gold"
    let partTwoSolution = parsedInput |> NumberOfBagsInside "shiny gold"
    uint partOneSolution, uint partTwoSolution
