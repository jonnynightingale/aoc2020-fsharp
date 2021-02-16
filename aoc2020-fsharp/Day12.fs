module Day12

type private Direction = North = 0 | East = 1 | South = 2 | West = 3

type private Instruction = MoveNorth | MoveEast | MoveSouth | MoveWest | TurnLeft | TurnRight | MoveForward

[<Struct>]
type private Position = { x : int; y : int }

[<Struct>]
type private ShipState = {
    position : Position
    waypoint : Position
    facing : Direction
}

[<Struct>]
type private Directive = {
    instruction : Instruction
    amount : int
}

let private RotateRight amount (state : ShipState) =
    let newDirection = state.facing |> int |> (+) (amount / 90) |> (fun x -> x % 4) |> enum<Direction>
    { state with facing = newDirection }

let private RotateLeft amount = RotateRight (360 - amount)

let private MoveInDirection amount direction (state : ShipState) =
    match direction with
    | Direction.North -> { state with position = { state.position with y = state.position.y + amount } }
    | Direction.East  -> { state with position = { state.position with x = state.position.x + amount } }
    | Direction.South -> { state with position = { state.position with y = state.position.y - amount } }
    | Direction.West  -> { state with position = { state.position with x = state.position.x - amount } }
    | _ -> state

let private MoveShip (state : ShipState) (directive : Directive) =
    match directive.instruction with
    | MoveNorth   -> state |> MoveInDirection directive.amount Direction.North
    | MoveEast    -> state |> MoveInDirection directive.amount Direction.East
    | MoveSouth   -> state |> MoveInDirection directive.amount Direction.South
    | MoveWest    -> state |> MoveInDirection directive.amount Direction.West
    | TurnLeft    -> state |> RotateLeft      directive.amount
    | TurnRight   -> state |> RotateRight     directive.amount
    | MoveForward -> state |> MoveInDirection directive.amount state.facing

let private MoveWaypoint amount direction (state : ShipState) =
    match direction with
    | Direction.North -> { state with waypoint = { state.waypoint with y = state.waypoint.y + amount } }
    | Direction.East  -> { state with waypoint = { state.waypoint with x = state.waypoint.x + amount } }
    | Direction.South -> { state with waypoint = { state.waypoint with y = state.waypoint.y - amount } }
    | Direction.West  -> { state with waypoint = { state.waypoint with x = state.waypoint.x - amount } }
    | _ -> state

let private RotateWaypointRight amount (state : ShipState) =
    match amount with
    |  90 -> { state with waypoint = { x =  state.waypoint.y; y = -state.waypoint.x } }
    | 180 -> { state with waypoint = { x = -state.waypoint.x; y = -state.waypoint.y } }
    | 270 -> { state with waypoint = { x = -state.waypoint.y; y =  state.waypoint.x } }
    | _   -> state

let private RotateWaypointLeft amount = RotateWaypointRight (360 - amount)

let private MoveTowardsWaypoint amount (state : ShipState) =
    let newPosition = {
        state.position with
            x = state.position.x + (amount * state.waypoint.x)
            y = state.position.y + (amount * state.waypoint.y)
    }
    { state with position = newPosition }

let private MoveShipWithWaypoint (state : ShipState) (directive : Directive) =
    match directive.instruction with
    | MoveNorth   -> state |> MoveWaypoint directive.amount Direction.North
    | MoveEast    -> state |> MoveWaypoint directive.amount Direction.East
    | MoveSouth   -> state |> MoveWaypoint directive.amount Direction.South
    | MoveWest    -> state |> MoveWaypoint directive.amount Direction.West
    | TurnLeft    -> state |> RotateWaypointLeft directive.amount
    | TurnRight   -> state |> RotateWaypointRight directive.amount
    | MoveForward -> state |> MoveTowardsWaypoint directive.amount

let private Parse input =
    let ParseLine (s : string) =
        let instruction =
            match s.Chars 0 with
            | 'N' -> MoveNorth
            | 'E' -> MoveEast
            | 'S' -> MoveSouth
            | 'W' -> MoveWest
            | 'L' -> TurnLeft
            | 'R' -> TurnRight
            |  _  -> MoveForward
        { instruction = instruction; amount = s.Substring 1 |> int }
    input |> Array.map ParseLine

let private ManhattanDistance (state : ShipState) = (abs state.position.x) + (abs state.position.y)

let Solve input =
    let directives = input |> Parse
    let initialState = {
        position = { x =  0; y = 0 };
        waypoint = { x = 10; y = 1 };
        facing = Direction.East
    }
    let partOneSolution = (initialState, directives) ||> Array.fold MoveShip |> ManhattanDistance
    let partTwoSolution = (initialState, directives) ||> Array.fold MoveShipWithWaypoint |> ManhattanDistance
    uint64 partOneSolution, uint64 partTwoSolution
