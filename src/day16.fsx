module Day16 =

    [<Struct>]
    type Rule = {
        FieldName : string
        Range1 : int * int
        Range2 : int * int
    }

    type Ticket = int array

    [<Struct>]
    type Input = {
        Rules : Rule array
        YourTicket : Ticket
        NearbyTickets : Ticket array
    }

    let parseRange (s : string) =
        let range = s.Split('-') |> Array.map int
        range.[0], range.[1]

    let parseRules =
        Array.map (fun (s : string) -> s.Split(':'))
        >> Array.map (fun ss -> Array.append (ss |> Array.take 1) (ss.[1].Split(' ')))
        >> Array.map (fun ss -> {
            FieldName = ss.[0];
            Range1 = ss.[2] |> parseRange;
            Range2 = ss.[4] |> parseRange;
        })

    let parseTicket (ticketInput : string) =
        ticketInput.Split(',') |> Array.map int

    let parse (input : string array) =
        let blankIndex = input |> Array.findIndex (String.length >> (=) 0)
        {
            Rules = input.[.. blankIndex - 1] |> parseRules
            YourTicket = input.[blankIndex + 2] |> parseTicket
            NearbyTickets = input.[blankIndex + 5 ..] |> Array.map parseTicket
        }

    let isValid (n : int) (rule : Rule) =
        let isInRange (min, max) = (n >= min) && (n <= max)
        (isInRange rule.Range1) || (isInRange rule.Range2)

    let errorRate rules ticket =
        ticket
        |> Array.filter (fun f -> rules |> Array.exists (isValid f) |> not)
        |> Array.sum

    let solvePartTwo input =
        let filterRulesForTicket ruleIndicesArr ticket : int array array =
            let filterRulesForValue (ruleIndices : int array) (n : int) : int array =
                ruleIndices |> Array.filter (fun i -> isValid n input.Rules.[i])

            Array.zip ruleIndicesArr ticket |> Array.map (fun (i, t) -> filterRulesForValue i t)

        let fieldCount = input.NearbyTickets |> Array.head |> Array.length
        let ruleIndices = Array.create fieldCount [| 0 .. fieldCount - 1 |]
        let validRulesForPosition =
            input.NearbyTickets
            |> Array.filter (errorRate input.Rules >> (=) 0)
            |> Array.fold filterRulesForTicket ruleIndices

        let foldPermutation state _ =
            let position = state |> fst |> Array.findIndex (Array.length >> (=) 1)
            let ruleIndex = state |> fst |> (fun x -> Array.get x position) |> Array.exactlyOne
            (
                state |> fst |> Array.map (Array.filter ((<>) ruleIndex)),
                (position, ruleIndex) :: (state |> snd)
            )

        let departurePositions =
            validRulesForPosition
            |> Array.fold foldPermutation (validRulesForPosition, [])
            |> snd
            |> List.filter (fun (_, ruleIndex) -> ruleIndex < 6)
            |> List.map fst

        input.YourTicket
        |> Array.indexed
        |> Array.filter (fun (i, _) -> departurePositions |> List.contains i)
        |> Array.map (snd >> uint64)
        |> Array.fold (*) 1UL

    let solve (input : string array) =
        let data = input |> parse
        let partOne = data.NearbyTickets |> Array.sumBy (errorRate data.Rules)
        let partTwo = solvePartTwo data
        string partOne, string partTwo
