module Day08

type private InstructionName = Acc | Jmp | Nop

[<Struct>]
type private Instruction =
    val name : InstructionName
    val value : int
    new (inName, inValue) = { name = inName; value = inValue }

[<Struct>]
type private ProgramState =
    val accumulator : int
    val instructionIndex : int
    new (acc, index) = { accumulator = acc; instructionIndex = index }

let private ParseLine (line : string) =
    let instructionName =
        match line.Substring(0, 3) with
        | "acc" -> Acc
        | "jmp" -> Jmp
        | _     -> Nop
    let value = line.Substring(4, line.Length - 4) |> int
    Instruction (instructionName, value)

let private Execute (state : ProgramState) (instruction : Instruction) =
    match instruction.name with
    | Acc -> ProgramState (state.accumulator + instruction.value, state.instructionIndex + 1)
    | Jmp -> ProgramState (state.accumulator, state.instructionIndex + instruction.value)
    | Nop -> ProgramState (state.accumulator, state.instructionIndex + 1)

let private AccumulatorValueBeforeLoop (program : Instruction array) =
    let rec Impl (executedInstructionIndices : int list) (state : ProgramState) =
        match executedInstructionIndices |> List.contains state.instructionIndex with
        | true -> state.accumulator
        | false ->
            program.[state.instructionIndex]
            |> Execute state
            |> (fun newState -> Impl (state.instructionIndex :: executedInstructionIndices) newState)
    let initialState = ProgramState(0, 0)
    Impl [] initialState

let Solve (input : string array) =
    let partOneSolution = input |> Array.map ParseLine |> AccumulatorValueBeforeLoop
    uint partOneSolution, 0u
