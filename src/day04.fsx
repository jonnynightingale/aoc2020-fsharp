module Day04 =

    open System.Text.RegularExpressions

    [<Struct>]
    type Passport =
        val mutable BirthYear : string
        val mutable IssueYear : string
        val mutable ExpirationYear : string
        val mutable Height : string
        val mutable HairColor : string
        val mutable EyeColor : string
        val mutable PassportID : string
        val mutable CountryID : string

    let validEyeColors = [| "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" |]

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

    let parseSingle input =
        let mutable passport = Passport()
        let parseField (line : string) =
            match line.Split ":" with
            | [| "byr"; s |] -> passport.BirthYear <- s
            | [| "iyr"; s |] -> passport.IssueYear <- s
            | [| "eyr"; s |] -> passport.ExpirationYear <- s
            | [| "hgt"; s |] -> passport.Height <- s
            | [| "hcl"; s |] -> passport.HairColor <- s
            | [| "ecl"; s |] -> passport.EyeColor <- s
            | [| "pid"; s |] -> passport.PassportID <- s
            | [| "cid"; s |] -> passport.CountryID <- s
            | _ -> ()
        input |> List.iter parseField
        passport

    let parse (input : string array) =
        let combineLines = List.collect (fun (s : string) -> s.Split " " |> Array.toList)
        input
        |> Array.toList
        |> divide (fun s -> s.Length > 0)
        |> List.map (combineLines >> parseSingle)

    let validateSimple (passport : Passport) =
        isNull passport.BirthYear |> not
        && isNull passport.IssueYear |> not
        && isNull passport.ExpirationYear |> not
        && isNull passport.Height |> not
        && isNull passport.HairColor |> not
        && isNull passport.EyeColor |> not
        && isNull passport.PassportID |> not

    let validateYear min max (y : string) = (y.Length = 4) && (int y |> (fun y -> y >= min && y <= max))

    let validateHeight (s : string) =
        if s.EndsWith("cm") then
            let isNumber, h = s.Substring (0, (s.Length - 2)) |> System.Int32.TryParse
            isNumber && h >= 150 && h <= 193
        elif s.EndsWith("in") then
            let isNumber, h = s.Substring (0, (s.Length - 2)) |> System.Int32.TryParse
            isNumber && h >= 59 && h <= 76
        else
            false

    let validateHairColor (s : string) = Regex.IsMatch (s, "^#[\da-f]{6}$")

    let validateEyeColor (s : string) = validEyeColors |> Array.contains s

    let validatePassportId (s : string) = Regex.IsMatch (s, "^\d{9}$")

    let validateComplex (passport : Passport) =
        validateYear 1920 2002 passport.BirthYear
        && validateYear 2010 2020 passport.IssueYear
        && validateYear 2020 2030 passport.ExpirationYear
        && validateHeight passport.Height
        && validateHairColor passport.HairColor
        && validateEyeColor passport.EyeColor
        && validatePassportId passport.PassportID

    let solve (input : string array) =
        let validPassportsSimple = input |> parse |> List.filter validateSimple
        let validPassportsComplex = validPassportsSimple |> List.filter validateComplex
        let partOneSolution = validPassportsSimple |> List.length
        let partTwoSolution = validPassportsComplex |> List.length
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day04.solve
printfn "Day 04: [ %i, %i ]" (fst solution) (snd solution)
