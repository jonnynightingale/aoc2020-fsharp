module Day05 =

    [<Struct>]
    type SeatLocation = {
        Row : int
        Column : int
    }
        //new (inRow, inColumn) = { row = inRow; column = inColumn }

    // Parse a single row of input into a seat location
    // E.g. "BFFFBBFRRR" -> row 70, column 7
    let parseLocation (inputRow : string) =
        let rowInput, columnInput = inputRow.ToCharArray() |> Array.splitAt 7
        let seatOffset backLetter maxIndex i c =
            if c = backLetter then 1 <<< (maxIndex - i) else 0
        {
            Row = rowInput |> Array.mapi (seatOffset 'B' 6) |> Array.sum
            Column = columnInput |> Array.mapi (seatOffset 'R' 2) |> Array.sum
        }

    let seatID (seatLocation : SeatLocation) = (seatLocation.Row * 8) + seatLocation.Column

    // Given a contiguous array of natural numbers that has been shuffled and had
    // one element removed, this function will calculate which number was removed
    // and returns it.
    let missingNumber maxValue (numbers : int array) =
        // We work out what the sum of all the numbers would be if no number was
        // missing. Subtracting the actual sum of the array from this value gives us
        // our solution.

        // Sum of the N consecutive numbers of which the max is 'max'
        let sumConsecutive n max =
            // Nth Triangular number (sum of all numbers up to N)
            let triangular n = (n * (n + 1)) >>> 1
            let exclusiveLowerBound = max - (n + 1)
            (triangular max) - (triangular exclusiveLowerBound)

        let sumWithoutMissingNumber = sumConsecutive numbers.Length maxValue
        let actualSum = numbers |> Array.sum
        sumWithoutMissingNumber - actualSum

    let solve (input : string array) =
        let seatIDs = input |> Array.map (parseLocation >> seatID)
        let maxId = seatIDs |> Array.max
        let missingSeatId = seatIDs |> missingNumber maxId
        string maxId, string missingSeatId
