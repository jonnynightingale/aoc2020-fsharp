module Day11 =

    type private SeatState = Floor | Empty | Occupied

    let private ParseCell c =
        match c with
        | 'L' -> Empty
        | '#' -> Occupied
        | _   -> Floor

    let private Parse (input : string array) =
        let width, height = input.[0].Length, input.Length
        let CoordToSeatState x y = (input.[y].Chars x) |> ParseCell
        Array2D.init width height CoordToSeatState

    let private neighborOffsets = [|
        (-1, -1); ( 0, -1); ( 1, -1);
        (-1,  0);           ( 1,  0);
        (-1,  1); ( 0,  1); ( 1,  1);
    |]

    let IsOffTheEdge w h x y = (x = -1) || (y = -1) || (x = w) || (y = h)

    let private NumberOfOccupiedNeighborsAt1 (roomState : SeatState [,]) width height x y =
        neighborOffsets
        |> Array.map (fun (xo, yo) -> x + xo, y + yo)
        |> Array.filter (fun (xn, yn) -> IsOffTheEdge width height xn yn |> not)
        |> Array.map (fun (xn, yn) -> roomState.[xn, yn])
        |> Array.filter ((=) Occupied)
        |> Array.length

    let private NumberOfOccupiedNeighborsAt2 (roomState : SeatState [,]) width height x y =
        let rec FirstSeatInDirectionIsOccupied x0 y0 (dx, dy) =
            let x1, y1 = x0 + dx, y0 + dy
            if IsOffTheEdge width height x1 y1 then
                false
            else
                match roomState.[x1,y1] with
                | Occupied -> true
                | Empty -> false
                | Floor -> FirstSeatInDirectionIsOccupied x1 y1 (dx, dy)
        neighborOffsets |> Array.filter (FirstSeatInDirectionIsOccupied x y) |> Array.length

    let private NextCellState threshold neighborCountFunc (roomState : SeatState [,]) width height x y seatState =
        match seatState, neighborCountFunc roomState width height x y with
        | Empty, n when n = 0 -> Occupied
        | Occupied, n when n >= threshold -> Empty
        | s, _ -> s

    let private NextRoomState nextCellState (roomState : SeatState [,]) =
        let width, height = roomState.GetLength 0, roomState.GetLength 1
        roomState |> Array2D.mapi (nextCellState roomState width height)

    let private Equal (a : SeatState [,]) (b : SeatState [,]) =
        (a |> Seq.cast<SeatState>, b |> Seq.cast<SeatState>)
        ||> Seq.compareWith (fun sa sb -> if sa = sb then 0 else 1)
        |> (=) 0

    let private IterateUntilStable nextCellState (initialRoomState : SeatState [,]) =
        let rec Impl roomState =
            let nextState = roomState |> NextRoomState nextCellState
            if Equal roomState nextState then roomState else Impl nextState
        Impl initialRoomState

    let private NumberOfOccupiedSeats (roomState : SeatState [,]) =
        roomState |> Seq.cast<SeatState> |> Seq.filter ((=) Occupied) |> Seq.length

    let Solve input =
        let initialRoomState = input |> Parse
        let partOneCellMapping = NextCellState 4 NumberOfOccupiedNeighborsAt1
        let partTwoCellMapping = NextCellState 5 NumberOfOccupiedNeighborsAt2
        let partOneSolution = initialRoomState |> (IterateUntilStable partOneCellMapping) |> NumberOfOccupiedSeats
        let partTwoSolution = initialRoomState |> (IterateUntilStable partTwoCellMapping) |> NumberOfOccupiedSeats
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day11.Solve
printfn "Day 11: [ %i, %i ]" (fst solution) (snd solution)
