module Day04

open System.Text.RegularExpressions

[<Struct>]
type private Passport =
    val mutable birthYear : string
    val mutable issueYear : string
    val mutable expirationYear : string
    val mutable height : string
    val mutable hairColor : string
    val mutable eyeColor : string
    val mutable passportId : string
    val mutable countryId : string

let private validEyeColors = [| "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" |]

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

let private ParseSingle input =
    let mutable passport = new Passport()
    let ParseField (line : string) =
        match line.Split ":" with
        | [| "byr"; s |] -> passport.birthYear <- s
        | [| "iyr"; s |] -> passport.issueYear <- s
        | [| "eyr"; s |] -> passport.expirationYear <- s
        | [| "hgt"; s |] -> passport.height <- s
        | [| "hcl"; s |] -> passport.hairColor <- s
        | [| "ecl"; s |] -> passport.eyeColor <- s
        | [| "pid"; s |] -> passport.passportId <- s
        | [| "cid"; s |] -> passport.countryId <- s
        | _ -> ()
    input |> List.iter ParseField
    passport

let private Parse (input : string array) =
    let CombineLines =
        List.collect (fun (s : string) -> s.Split " " |> Array.toList)
    input
    |> Array.toList
    |> Divide (fun s -> s.Length > 0)
    |> List.map CombineLines
    |> List.map ParseSingle

let private ValidateSimple (passport : Passport) =
    isNull passport.birthYear |> not
    && isNull passport.issueYear |> not
    && isNull passport.expirationYear |> not
    && isNull passport.height |> not
    && isNull passport.hairColor |> not
    && isNull passport.eyeColor |> not
    && isNull passport.passportId |> not

let ValidateYear min max (y : string) =
    (y.Length = 4) && (int y |> (fun y -> y >= min && y <= max))

let private ValidateHeight (s : string) =
    if s.EndsWith("cm") then
        let isNumber, h = s.Substring (0, (s.Length - 2)) |> System.Int32.TryParse
        isNumber && h >= 150 && h <= 193
    elif s.EndsWith("in") then
        let isNumber, h = s.Substring (0, (s.Length - 2)) |> System.Int32.TryParse
        isNumber && h >= 59 && h <= 76
    else
        false

let private ValidateHairColor (s : string) = Regex.IsMatch (s, "^#[\da-f]{6}$")

let private ValidateEyeColor (s : string) = validEyeColors |> Array.contains s

let private ValidatePassportId (s : string) = Regex.IsMatch (s, "^\d{9}$")

let private ValidateComplex (passport : Passport) =
    ValidateYear 1920 2002 passport.birthYear
    && ValidateYear 2010 2020 passport.issueYear
    && ValidateYear 2020 2030 passport.expirationYear
    && ValidateHeight passport.height
    && ValidateHairColor passport.hairColor
    && ValidateEyeColor passport.eyeColor
    && ValidatePassportId passport.passportId

let Solve (input : string array) =
    let validPassportsSimple = input |> Parse |> List.filter ValidateSimple
    let validPassportsComplex = validPassportsSimple |> List.filter ValidateComplex
    let partOneSolution = validPassportsSimple |> List.length
    let partTwoSolution = validPassportsComplex |> List.length
    uint64 partOneSolution, uint64 partTwoSolution
