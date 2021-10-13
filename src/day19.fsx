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

    let parseRules = Array.map parseRule

    let parseMessages = Array.map (fun (m : string) -> m.ToCharArray ())

    let parse input =
        let blankIndex = input |> Array.findIndex ((=) "")
        parseRules input.[..blankIndex - 1], parseMessages input.[blankIndex + 1..]

    let isValid rules message =
        let getRule index = rules |> Array.find (fun r -> r.Index = index)

        let rec isValidForRule message rule =
            let areValidForRule mArr rule = mArr |> Array.collect (fun m -> isValidForRule m rule)
            
            let isValidForSubRule message subRule =
                match subRule with
                | Letter l ->
                    message
                    |> Array.tryHead
                    |> Option.filter ((=) l)
                    |> Option.map (fun _ -> message.[1..])
                    |> Option.toArray
                | RuleNumbers ns ->
                    ns
                    |> Array.map getRule
                    |> Array.fold areValidForRule [| message |]

            rule.SubRules |> Array.collect (isValidForSubRule message)
        
        0 |> getRule |> isValidForRule message |> Array.exists Array.isEmpty

    let solve input =
        let rules, messages = parse input
        let partOne = messages |> Array.filter (isValid rules) |> Array.length
        let rules2 = rules |> Array.map (fun rule ->
            match rule.Index with
            | 8 -> { rule with SubRules = [| RuleNumbers [| 42 |]; RuleNumbers [| 42; 8 |] |] }
            | 11 -> { rule with SubRules = [| RuleNumbers [| 42; 31 |]; RuleNumbers [| 42; 11; 31 |] |] }
            | _ -> rule
        )
        let partTwo = messages |> Array.filter (isValid rules2) |> Array.length
        string partOne, string partTwo
