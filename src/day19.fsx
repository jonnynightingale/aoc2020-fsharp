module Day19 =

    type SubRule = Letter of char | RuleNumbers of int array

    type Rule = {
        Index : int
        SubRules : SubRule array
    }

    let parseSubRule (input : string) =
        match input.[0] with
        | '"' -> input.[1] |> Letter
        | _ -> (input.Split ' ') |> Array.map int |> RuleNumbers

    let parseRule (input : string) =
        let colonIndex = input.IndexOf ':'
        {
            Index = input.[..colonIndex - 1] |> int
            SubRules = (input.[colonIndex + 1..].Split '|') |> Array.map (fun s -> s.Trim ()) |> Array.map parseSubRule
        }

    let parseRules = Array.map parseRule >> Array.sortBy (fun rule -> rule.Index)

    let parseMessages = Array.map (fun (m : string) -> m.ToCharArray ())

    let parse input =
        let blankIndex = input |> Array.findIndex ((=) "")
        parseRules input.[..blankIndex - 1], parseMessages input.[blankIndex + 1..]

    let isValid rules message =
        let rec isValidForRule message rule =
            let areValidForRule mArr rule = mArr |> Array.collect (fun m -> isValidForRule m rule)
            
            let isValidForSubRule message subRule =
                match subRule with
                | Letter l -> message |> Array.tryHead |> Option.filter ((=) l) |> Option.map (fun _ -> message.[1..]) |> Option.toArray
                | RuleNumbers ns -> ns |> Array.map (rules |> Array.get) |> Array.fold areValidForRule [| message |]

            rule.SubRules |> Array.collect (isValidForSubRule message)
        
        isValidForRule message rules.[0] |> Array.exists Array.isEmpty

    let solve input =
        let rules, messages = parse input
        let partOne = messages |> Array.filter (isValid rules) |> Array.length
        let rules2 = rules |> Array.mapi (fun i rule ->
            match i with
            | 8 -> { Index = 8; SubRules = [| RuleNumbers [| 42 |]; RuleNumbers [| 42; 8 |] |] }
            | 11 -> { Index = 11; SubRules = [| RuleNumbers [| 42; 31 |]; RuleNumbers [| 42; 11; 31 |] |] }
            | _ -> rule
        )
        let partTwo = messages |> Array.filter (isValid rules2) |> Array.length
        string partOne, string partTwo
