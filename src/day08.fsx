module Day08 =

    type InstructionName = Acc | Jmp | Nop

    [<Struct>]
    type Instruction = {
        Name : InstructionName
        Value : int
    }

    [<Struct>]
    type ProgramState = {
        Accumulator : int
        InstructionIndex : int
    }

    let initialState = { Accumulator = 0; InstructionIndex = 0 }

    type HaltReason = LoopDetected | Complete

    let parseLine (line : string) =
        let instructionName =
            match line.Substring(0, 3) with
            | "acc" -> Acc
            | "jmp" -> Jmp
            | _     -> Nop
        let value = line.Substring(4, line.Length - 4) |> int
        { Name = instructionName; Value = value }

    let execute state instruction =
        match instruction.Name with
        | Acc -> { Accumulator = state.Accumulator + instruction.Value; InstructionIndex = state.InstructionIndex + 1 }
        | Jmp -> { state with InstructionIndex = state.InstructionIndex + instruction.Value }
        | Nop -> { state with InstructionIndex = state.InstructionIndex + 1 }

    let run state (program : Instruction array) =
        let rec runImpl (executedInstructions : int list) (state : ProgramState) =
            if state.InstructionIndex = program.Length then
                Complete, state
            elif executedInstructions |> List.contains state.InstructionIndex then
                LoopDetected, state
            else
                program.[state.InstructionIndex]
                |> execute state
                |> runImpl (state.InstructionIndex :: executedInstructions)
        runImpl [] state

    let canBeAltered = function
        | Jmp | Nop -> true
        | _ -> false

    let alter i = { Name = (if i.Name = Jmp then Nop else Jmp); Value = i.Value }

    let fixProgramAndRun (program : Instruction array) =
        let rec impl (state : ProgramState) =
            let currentInstruction = program.[state.InstructionIndex]
            if currentInstruction.Name |> canBeAltered then
                let alteredProgram = program |> Array.mapi (fun idx ins ->
                    if idx = state.InstructionIndex then alter ins else ins)
                match alteredProgram |> run state with
                | Complete, finalState -> finalState
                | _ -> currentInstruction |> execute state |> impl
            else
                currentInstruction |> execute state |> impl
        impl initialState

    let solve (input : string array) =
        let program = input |> Array.map parseLine
        let partOne = program |> run initialState |> snd |> (fun i -> i.Accumulator)
        let partTwo = program |> fixProgramAndRun |> (fun i -> i.Accumulator)
        string partOne, string partTwo
