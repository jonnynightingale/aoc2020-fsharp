module Day02

type private Policy = {
    min : int
    max : int
    letter : char
}

let private ParseInputLine (line : string) =
    let split = line.Split ' '
    let minMax = split.[0].Split '-' |> Array.map int
    let min = int minMax.[0]
    let max = int minMax.[1]
    let letter = split.[1].Chars 0
    let password = split.[2]
    { min = min; max = max; letter = letter }, password

let private ValidatePasswordRuleOne (policy, (password : string)) =
    password.ToCharArray()
    |> Array.filter (fun c -> c = policy.letter)
    |> Array.length
    |> (fun n -> n >= policy.min && n <= policy.max)

let private ValidatePasswordRuleTwo (policy, (password : string)) =
    let matchingCharacterCount =
        [| policy.min; policy.max |]
        |> Array.filter (fun n -> password.[n-1] = policy.letter)
        |> Array.length
    matchingCharacterCount = 1

let private CountValidPasswords policiesAndPasswords isValid =
    policiesAndPasswords
    |> Array.filter isValid
    |> Array.length

let Solve (input : string array) =
    let policiesAndPasswords = input |> Array.map ParseInputLine
    let partOneSolution = CountValidPasswords policiesAndPasswords ValidatePasswordRuleOne
    let partTwoSolution = CountValidPasswords policiesAndPasswords ValidatePasswordRuleTwo
    partOneSolution |> uint, partTwoSolution |> uint
