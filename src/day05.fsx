module Day05 =

    [<Struct>]
    type private SeatLocation =
        val row : int
        val column : int
        new (inRow, inColumn) = { row = inRow; column = inColumn }

    // Parse a single row of input into a seat location
    // E.g. "BFFFBBFRRR" -> row 70, column 7
    let private ParseLocation (inputRow : string) =
        let rowInput, columnInput = inputRow.ToCharArray() |> Array.splitAt 7
        let SeatOffset backLetter maxIndex i c =
            if c = backLetter then 1 <<< (maxIndex - i) else 0
        new SeatLocation(
            rowInput |> Array.mapi (SeatOffset 'B' 6) |> Array.sum,
            columnInput |> Array.mapi (SeatOffset 'R' 2) |> Array.sum
        )

    let private SeatId (seatLocation : SeatLocation) =
        (seatLocation.row * 8) + seatLocation.column

    // Given a contiguous array of natural numbers that has been shuffled and had
    // one element removed, this function will calculate which number was removed
    // and returns it.
    let private MissingNumber maxValue (numbers : int array) =
        // We work out what the sum of all the numbers would be if no number was
        // missing. Subtracting the actual sum of the array from this value gives us
        // our solution.

        // Sum of the N consecutive numbers of which the max is 'max'
        let SumConsecutive n max =
            // Nth Triangular number (sum of all numbers up to N)
            let Triangular n = (n * (n + 1)) >>> 1
            let exclusiveLowerBound = max - (n + 1)
            (Triangular max) - (Triangular exclusiveLowerBound)

        let sumWithoutMissingNumber = SumConsecutive numbers.Length maxValue
        let actualSum = numbers |> Array.sum
        sumWithoutMissingNumber - actualSum

    let Solve (input : string array) =
        let seatIds = input |> Array.map (ParseLocation >> SeatId)
        let maxId = seatIds |> Array.max
        let missingSeatId = seatIds |> MissingNumber maxId
        uint64 maxId, uint64 missingSeatId

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day05.Solve
printfn "Day 05: [ %i, %i ]" (fst solution) (snd solution)
