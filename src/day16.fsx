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
        {
            Rules = input.[0..19] |> parseRules
            YourTicket = input.[22] |> parseTicket
            NearbyTickets = input.[25..] |> Array.map parseTicket
        }

    let isValid (n : int) (rule : Rule) =
        let isInRange (min, max) = (n >= min) && (n <= max)
        (isInRange rule.Range1) || (isInRange rule.Range2)

    let errorRate rules ticket =
        ticket
        |> Array.filter (fun f -> rules |> Array.exists (isValid f) |> not)
        |> Array.sum

    let solvePartTwo rules tickets =
        tickets
        |> Array.filter (fun t -> t |> errorRate rules |> (=) 0)
        // TODO: Determine which field is which

    let solve (input : string array) =
        let data = input |> parse
        let partOne = data.NearbyTickets |> Array.sumBy (errorRate data.Rules)
        uint64 partOne, uint64 0

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day16.solve
printfn "Day 16: [ %i, %i ]" (fst solution) (snd solution)
