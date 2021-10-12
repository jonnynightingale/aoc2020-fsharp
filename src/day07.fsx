module Day07 =

    open System.Text.RegularExpressions

    let parseLine line =
        let parentMatch = Regex.Match (line, "^(.+?) bags")
        let parent = parentMatch.Groups.[1].Value
        let childrenMatches = Regex.Matches (line, "(\d+) (.+?) bag")
        let children =
            childrenMatches
            |> Seq.cast
            |> Seq.map (fun (regMatch : Match) -> int regMatch.Groups.[1].Value, regMatch.Groups.[2].Value)
            |> Seq.toArray
        parent, children

    let parse = Array.map parseLine >> Map.ofArray

    let numberOfOutermostBagPossibilities bagType lookup =
        let rec bagsThatCanHold bagType =
            let bagsThatCanHoldThis =
                lookup
                |> Map.filter (fun _ x -> x |> Array.exists (fun (_, bag) -> bag = bagType))
                |> Map.toList
                |> List.map fst
            match bagsThatCanHoldThis.IsEmpty with
            | true -> [ bagType ]
            | false ->
                bagsThatCanHoldThis
                |> List.fold (fun acc b -> b |> bagsThatCanHold |> List.append acc) [ bagType ]

        bagType |> bagsThatCanHold |> List.distinct |> List.length |> (+) -1

    let numberOfBagsInside bagType (lookup : Map<string, (int * string)[]>) =
        let rec numberOfBagsInsideImpl bagType =
            lookup.[bagType] |> Array.sumBy (fun (n, b) -> n + (n * numberOfBagsInsideImpl b))
        numberOfBagsInsideImpl bagType

    let solve (input : string array) =
        let parsedInput = input |> parse
        let partOne = parsedInput |> numberOfOutermostBagPossibilities "shiny gold"
        let partTwo = parsedInput |> numberOfBagsInside "shiny gold"
        string partOne, string partTwo
