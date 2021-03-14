module Day14 =

    type BitInstruction = Zero | One | X

    type private Instruction =
        | SetBitmask of BitInstruction array
        | WriteMemory of uint64 * uint64

    [<Struct>]
    type private ProgramState = {
        mask : BitInstruction array
        memory : Map<uint64, uint64>
    }

    let private ParseBitmask (input : string) =
        input.ToCharArray()
        |> Array.rev
        |> Array.map (fun c -> match c with '0' -> Zero | '1' -> One | _ -> X)
        |> Instruction.SetBitmask

    let private ParseMemoryWrite (input : string) =
        (
            input.Substring(4, (input.IndexOf ']') - 4) |> uint64,
            input.Substring(input.IndexOf '=' + 2) |> uint64
        )
        |> Instruction.WriteMemory

    let private Parse input =
        let ParseLine (l : string) =
            match l.Chars 1 with
            | 'a' -> l.Substring 7 |> ParseBitmask
            | _ -> l |> ParseMemoryWrite
        input |> Array.map ParseLine

    let private SetBitNToZero n value = value &&& ~~~(1UL <<< n)
    let private SetBitNToOne  n value = value ||| (1UL <<< n)
    let private SetBitNToMatchSource n src value =
        match src &&& (1UL <<< n) with
        | 0UL -> SetBitNToZero n value
        | _   -> SetBitNToOne  n value

    let private ApplyBitmask mask n =
        let DoSingleBit res (i, bi) =
            match bi with
            | Zero -> res |> SetBitNToZero i
            | One -> res |> SetBitNToOne i
            | X -> res
        mask |> Array.indexed |> Array.fold DoSingleBit n

    let private DoSetMemory (addr, value) state =
        let maskedValue = value |> ApplyBitmask state.mask
        { state with memory = state.memory |> Map.add addr maskedValue }

    let private RunProgram initialState instructions =
        let RunStep state instruction =
            match instruction with
            | SetBitmask mask -> { state with mask = mask }
            | WriteMemory (addr, value) -> state |> DoSetMemory (addr, value)
        instructions |> Array.fold RunStep initialState

    let private GetFloatingAddresses addr mask =
        let SplitAtIndex index n =
            [| n |> SetBitNToZero index; n |> SetBitNToOne index |]
        let DoSingleBit res (i, bi) =
            match bi with
            | Zero -> res |> Array.map (SetBitNToMatchSource i addr)
            | One -> res |> Array.map (SetBitNToOne i)
            | X -> res |> Array.collect (SplitAtIndex i)
        mask |> Array.indexed |> Array.fold DoSingleBit [| 0UL |]

    let private RunProgramV2 initialState instructions =
        let RunStep state instruction =
            match instruction with
            | SetBitmask mask -> { state with mask = mask }
            | WriteMemory (addr, value) ->
                let addresses = GetFloatingAddresses addr state.mask
                let newMemory = addresses |> Array.fold (fun mem a -> mem |> Map.add a value) state.memory
                { state with memory = newMemory }
        instructions |> Array.fold RunStep initialState

    let private SumOfValuesInMemory state =
        state.memory |> Map.fold (fun sum _ value -> sum + value) 0UL

    let Solve input =
        let instructions = input |> Parse
        let initialState = { mask = Array.empty; memory = Map.empty }
        let partOneSolution = instructions |> RunProgram initialState |> SumOfValuesInMemory
        let partTwoSolution = instructions |> RunProgramV2 initialState |> SumOfValuesInMemory
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day14.Solve
printfn "Day 14: [ %i, %i ]" (fst solution) (snd solution)
