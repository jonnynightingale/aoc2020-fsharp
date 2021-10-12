module Day11 =

    type SeatState = Floor | Empty | Occupied

    let parseCell = function | 'L' -> Empty | '#' -> Occupied | _ -> Floor

    let parse (input : string array) =
        let width, height = input.[0].Length, input.Length
        let coordToSeatState x y = (input.[y].Chars x) |> parseCell
        Array2D.init width height coordToSeatState

    let neighborOffsets = [|
        (-1, -1); ( 0, -1); ( 1, -1);
        (-1,  0);           ( 1,  0);
        (-1,  1); ( 0,  1); ( 1,  1);
    |]

    let isOffTheEdge w h x y = (x = -1) || (y = -1) || (x = w) || (y = h)

    let numberOfOccupiedNeighborsAt1 (roomState : SeatState [,]) width height x y =
        neighborOffsets
        |> Array.map (fun (xo, yo) -> x + xo, y + yo)
        |> Array.filter (fun (xn, yn) -> isOffTheEdge width height xn yn |> not)
        |> Array.map (fun (xn, yn) -> roomState.[xn, yn])
        |> Array.filter ((=) Occupied)
        |> Array.length

    let numberOfOccupiedNeighborsAt2 (roomState : SeatState [,]) width height x y =
        let rec firstSeatInDirectionIsOccupied x0 y0 (dx, dy) =
            let x1, y1 = x0 + dx, y0 + dy
            if isOffTheEdge width height x1 y1 then
                false
            else
                match roomState.[x1,y1] with
                | Occupied -> true
                | Empty -> false
                | Floor -> firstSeatInDirectionIsOccupied x1 y1 (dx, dy)
        neighborOffsets |> Array.filter (firstSeatInDirectionIsOccupied x y) |> Array.length

    let nextCellState threshold neighborCountFunc roomState width height x y seatState =
        match seatState, neighborCountFunc roomState width height x y with
        | Empty, n when n = 0 -> Occupied
        | Occupied, n when n >= threshold -> Empty
        | s, _ -> s

    let nextRoomState nextCellState (roomState : SeatState [,]) =
        let width, height = roomState.GetLength 0, roomState.GetLength 1
        roomState |> Array2D.mapi (nextCellState roomState width height)

    let equal a b =
        (a |> Seq.cast<SeatState>, b |> Seq.cast<SeatState>)
        ||> Seq.compareWith (fun sa sb -> if sa = sb then 0 else 1)
        |> (=) 0

    let iterateUntilStable nextCellState initialRoomState =
        let rec impl roomState =
            let nextState = roomState |> nextRoomState nextCellState
            if equal roomState nextState then roomState else impl nextState
        impl initialRoomState

    let numberOfOccupiedSeats roomState =
        roomState |> Seq.cast<SeatState> |> Seq.filter ((=) Occupied) |> Seq.length

    let solve input =
        let initialRoomState = input |> parse
        let partOneCellMapping = nextCellState 4 numberOfOccupiedNeighborsAt1
        let partTwoCellMapping = nextCellState 5 numberOfOccupiedNeighborsAt2
        let partOne = initialRoomState |> (iterateUntilStable partOneCellMapping) |> numberOfOccupiedSeats
        let partTwo = initialRoomState |> (iterateUntilStable partTwoCellMapping) |> numberOfOccupiedSeats
        uint64 partOne, uint64 partTwo

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day11.solve
printfn "Day 11: [ %i, %i ]" (fst solution) (snd solution)
