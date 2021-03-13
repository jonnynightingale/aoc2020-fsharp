module Day16

[<Struct>]
type private Rule = {
    fieldName : string
    range1 : int * int
    range2 : int * int
}

[<Struct>]
type private Ticket = {
    fields : int array
}

[<Struct>]
type private Input = {
    rules : Rule array
    yourTicket : Ticket
    nearbyTickets : Ticket array
}

let private ParseRange (s : string) =
    let range = s.Split('-') |> Array.map int
    range.[0], range.[1]

let private ParseRules (rulesInput : string array) =
    rulesInput
    |> Array.map (fun s -> s.Split(':'))
    |> Array.map (fun ss -> Array.append (ss |> Array.take 1) (ss.[1].Split(' ')))
    |> Array.map (fun ss -> {
        fieldName = ss.[0];
        range1 = ss.[2] |> ParseRange;
        range2 = ss.[4] |> ParseRange;
    })

let private ParseTicket (ticketInput : string) =
    { fields = ticketInput.Split(',') |> Array.map int }

let private Parse (input : string array) =
    {
        rules = input.[0..19] |> ParseRules
        yourTicket = input.[22] |> ParseTicket
        nearbyTickets = input.[25..] |> Array.map ParseTicket
    }

let private IsValid (n : int) (rule : Rule) =
    let IsInRange (min, max) = (n >= min) && (n <= max)
    (IsInRange rule.range1) || (IsInRange rule.range2)

let private ErrorRate (rules : Rule array) (ticket : Ticket) =
    ticket.fields
    |> Array.filter (fun f -> rules |> Array.exists (IsValid f) |> not)
    |> Array.sum

let Solve (input : string array) =
    let data = input |> Parse
    let partOneSolution = data.nearbyTickets |> Array.map (ErrorRate data.rules) |> Array.sum
    uint64 partOneSolution, uint64 0
