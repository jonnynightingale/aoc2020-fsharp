module Day04

type private Field = { id : string; required : bool }

let private fields = [|
    { id = "byr"; required = true  }  // Birth Year
    { id = "iyr"; required = true  }  // Issue Year
    { id = "eyr"; required = true  }  // Expiration Year
    { id = "hgt"; required = true  }  // Height
    { id = "hcl"; required = true  }  // Hair Color
    { id = "ecl"; required = true  }  // Eye Color
    { id = "pid"; required = true  }  // Passport ID
    { id = "cid"; required = false }  // Country ID
|]

let private requiredFields = fields |> Array.filter (fun f -> f.required)

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

let private Parse (input : string array) =
    let CombineLines =
        List.collect (fun (s : string) -> s.Split " " |> Array.toList)
    input
    |> Array.toList
    |> Divide (fun s -> s.Length > 0)
    |> List.map CombineLines

let Solve (input : string array) =
    Parse input
    0u, 0u
