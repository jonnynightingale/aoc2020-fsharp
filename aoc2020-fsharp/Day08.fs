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

let private initialState = ProgramState(0, 0)

type private HaltReason = LoopDetected | Complete

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

let private Run (state : ProgramState) (program : Instruction array) =
    let rec RunImpl (executedInstructions : int list) (state : ProgramState) =
        if state.instructionIndex = program.Length then
            Complete, state
        elif executedInstructions |> List.contains state.instructionIndex then
            LoopDetected, state
        else
            program.[state.instructionIndex]
            |> Execute state
            |> RunImpl (state.instructionIndex :: executedInstructions)
    RunImpl [] state

let private CanBeAltered (instruction : Instruction) =
    match instruction.name with
    | Jmp | Nop -> true
    | _ -> false

let private Alter (i : Instruction) = Instruction ((if i.name = Jmp then Nop else Jmp), i.value)

let private FixProgramAndRun (program : Instruction array) =
    let rec Impl (state : ProgramState) =
        let currentInstruction = program.[state.instructionIndex]
        if currentInstruction |> CanBeAltered then
            let alteredProgram = program |> Array.mapi (fun idx ins ->
                if idx = state.instructionIndex then Alter ins else ins)
            match alteredProgram |> Run state with
            | Complete, finalState -> finalState
            | _ -> currentInstruction |> Execute state |> Impl
        else
            currentInstruction |> Execute state |> Impl
    Impl initialState

let Solve (input : string array) =
    let program = input |> Array.map ParseLine
    let partOneSolution = program |> Run initialState |> snd |> (fun i -> i.accumulator)
    let partTwoSolution = program |> FixProgramAndRun |> (fun i -> i.accumulator)
    uint64 partOneSolution, uint64 partTwoSolution
