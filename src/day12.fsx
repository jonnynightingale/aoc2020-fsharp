module Day12 =

    type Direction = North = 0 | East = 1 | South = 2 | West = 3

    type Instruction = MoveNorth | MoveEast | MoveSouth | MoveWest | TurnLeft | TurnRight | MoveForward

    [<Struct>]
    type Position = { X : int; Y : int }

    [<Struct>]
    type ShipState = {
        Position : Position
        Waypoint : Position
        Facing : Direction
    }

    [<Struct>]
    type Directive = {
        Instruction : Instruction
        Amount : int
    }

    let rotateRight amount (state : ShipState) =
        let newDirection = state.Facing |> int |> (+) (amount / 90) |> (fun x -> x % 4) |> enum<Direction>
        { state with Facing = newDirection }

    let rotateLeft amount = rotateRight (360 - amount)

    let moveInDirection amount direction (state : ShipState) =
        match direction with
        | Direction.North -> { state with Position = { state.Position with Y = state.Position.Y + amount } }
        | Direction.East  -> { state with Position = { state.Position with X = state.Position.X + amount } }
        | Direction.South -> { state with Position = { state.Position with Y = state.Position.Y - amount } }
        | Direction.West  -> { state with Position = { state.Position with X = state.Position.X - amount } }
        | _ -> state

    let moveShip (state : ShipState) (directive : Directive) =
        match directive.Instruction with
        | MoveNorth   -> state |> moveInDirection directive.Amount Direction.North
        | MoveEast    -> state |> moveInDirection directive.Amount Direction.East
        | MoveSouth   -> state |> moveInDirection directive.Amount Direction.South
        | MoveWest    -> state |> moveInDirection directive.Amount Direction.West
        | TurnLeft    -> state |> rotateLeft      directive.Amount
        | TurnRight   -> state |> rotateRight     directive.Amount
        | MoveForward -> state |> moveInDirection directive.Amount state.Facing

    let moveWaypoint amount direction (state : ShipState) =
        match direction with
        | Direction.North -> { state with Waypoint = { state.Waypoint with Y = state.Waypoint.Y + amount } }
        | Direction.East  -> { state with Waypoint = { state.Waypoint with X = state.Waypoint.X + amount } }
        | Direction.South -> { state with Waypoint = { state.Waypoint with Y = state.Waypoint.Y - amount } }
        | Direction.West  -> { state with Waypoint = { state.Waypoint with X = state.Waypoint.X - amount } }
        | _ -> state

    let rotateWaypointRight amount (state : ShipState) =
        match amount with
        |  90 -> { state with Waypoint = { X =  state.Waypoint.Y; Y = -state.Waypoint.X } }
        | 180 -> { state with Waypoint = { X = -state.Waypoint.X; Y = -state.Waypoint.Y } }
        | 270 -> { state with Waypoint = { X = -state.Waypoint.Y; Y =  state.Waypoint.X } }
        | _   -> state

    let rotateWaypointLeft amount = rotateWaypointRight (360 - amount)

    let moveTowardsWaypoint amount (state : ShipState) =
        let newPosition = {
            state.Position with
                X = state.Position.X + (amount * state.Waypoint.X)
                Y = state.Position.Y + (amount * state.Waypoint.Y)
        }
        { state with Position = newPosition }

    let moveShipWithWaypoint (state : ShipState) (directive : Directive) =
        match directive.Instruction with
        | MoveNorth   -> state |> moveWaypoint directive.Amount Direction.North
        | MoveEast    -> state |> moveWaypoint directive.Amount Direction.East
        | MoveSouth   -> state |> moveWaypoint directive.Amount Direction.South
        | MoveWest    -> state |> moveWaypoint directive.Amount Direction.West
        | TurnLeft    -> state |> rotateWaypointLeft directive.Amount
        | TurnRight   -> state |> rotateWaypointRight directive.Amount
        | MoveForward -> state |> moveTowardsWaypoint directive.Amount

    let parse input =
        let parseLine (s : string) =
            let instruction =
                match s.Chars 0 with
                | 'N' -> MoveNorth
                | 'E' -> MoveEast
                | 'S' -> MoveSouth
                | 'W' -> MoveWest
                | 'L' -> TurnLeft
                | 'R' -> TurnRight
                |  _  -> MoveForward
            { Instruction = instruction; Amount = s.Substring 1 |> int }
        input |> Array.map parseLine

    let manhattanDistance (state : ShipState) = (abs state.Position.X) + (abs state.Position.Y)

    let solve input =
        let directives = input |> parse
        let initialState = {
            Position = { X =  0; Y = 0 };
            Waypoint = { X = 10; Y = 1 };
            Facing = Direction.East
        }
        let partOne = (initialState, directives) ||> Array.fold moveShip |> manhattanDistance
        let partTwo = (initialState, directives) ||> Array.fold moveShipWithWaypoint |> manhattanDistance
        string partOne, string partTwo
