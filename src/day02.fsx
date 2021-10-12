module Day02 =

    type Policy = {
        Min : int
        Max : int
        Letter : char
    }

    let parseInputLine (line : string) =
        let split = line.Split ' '
        let minMax = split.[0].Split '-' |> Array.map int
        {
            Min = int minMax.[0]
            Max = int minMax.[1]
            Letter = split.[1].Chars 0
        }, split.[2]

    let validatePasswordRuleOne (policy, (password : string)) =
        password.ToCharArray()
        |> Array.filter (fun c -> c = policy.Letter)
        |> Array.length
        |> (fun n -> n >= policy.Min && n <= policy.Max)

    let validatePasswordRuleTwo (policy, (password : string)) =
        let matchingCharacterCount =
            [| policy.Min; policy.Max |]
            |> Array.filter (fun n -> password.[n - 1] = policy.Letter)
            |> Array.length
        matchingCharacterCount = 1

    let countValidPasswords isValid policiesAndPasswords = policiesAndPasswords |> Array.filter isValid |> Array.length

    let solve (input : string array) =
        let policiesAndPasswords = input |> Array.map parseInputLine
        let partOne = policiesAndPasswords |> countValidPasswords validatePasswordRuleOne
        let partTwo = policiesAndPasswords |> countValidPasswords validatePasswordRuleTwo
        string partOne, string partTwo
