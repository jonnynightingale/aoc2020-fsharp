module Day12

type private FacingDirection = North | East | South | West

type private Instruction =
    MoveNorth | MoveEast | MoveSouth | MoveWest | TurnLeft | TurnRight | MoveForward

[<Struct>]
type private ShipState =
    val x : int
    val y : int
    val facing : FacingDirection
    new (inX, inY, inFacing) = { x = inX; y = inY; facing = inFacing }

[<Struct>]
type private Directive =
    val instruction : Instruction
    val amount : int
    new (inInstruction, inAmount) = { instruction = inInstruction; amount = inAmount }

let private IncrementDirection direction =
    match direction with
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North

let private RotateRight amount direction =
    match amount with
    |  90 -> direction |> IncrementDirection
    | 180 -> direction |> IncrementDirection |> IncrementDirection
    | 270 -> direction |> IncrementDirection |> IncrementDirection |> IncrementDirection
    | _   -> direction

let private RotateLeft amount = RotateRight (360 - amount)

let private MoveInDirection direction amount (state : ShipState) =
    match direction with
    | North   -> new ShipState(state.x,          state.y + amount, state.facing)
    | East    -> new ShipState(state.x + amount, state.y,          state.facing)
    | South   -> new ShipState(state.x,          state.y - amount, state.facing)
    | West    -> new ShipState(state.x - amount, state.y,          state.facing)

let private MoveShip (state : ShipState) (directive : Directive) =
    match directive.instruction with
    | MoveNorth   -> state |> MoveInDirection North directive.amount
    | MoveEast    -> state |> MoveInDirection East  directive.amount
    | MoveSouth   -> state |> MoveInDirection South directive.amount
    | MoveWest    -> state |> MoveInDirection West  directive.amount
    | TurnLeft    -> new ShipState(state.x, state.y, state.facing |> RotateLeft  directive.amount)
    | TurnRight   -> new ShipState(state.x, state.y, state.facing |> RotateRight directive.amount)
    | MoveForward -> state |> MoveInDirection state.facing directive.amount

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
        new Directive(instruction, s.Substring 1 |> int)
    input |> Array.map ParseLine

let private ManhattanDistance (state : ShipState) = (abs state.x) + (abs state.y)

let Solve input =
    let directives = input |> Parse
    let initialState = new ShipState(0, 0, East)
    let partOneSolution = (initialState, directives) ||> Array.fold MoveShip |> ManhattanDistance
    uint64 partOneSolution, uint64 0
