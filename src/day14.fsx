module Day14 =

    type BitInstruction = Zero | One | X

    type Instruction =
        | SetBitmask of BitInstruction array
        | WriteMemory of uint64 * uint64

    [<Struct>]
    type ProgramState = {
        Mask : BitInstruction array
        Memory : Map<uint64, uint64>
    }

    let parseBitmask (input : string) =
        input.ToCharArray()
        |> Array.rev
        |> Array.map (fun c -> match c with '0' -> Zero | '1' -> One | _ -> X)
        |> Instruction.SetBitmask

    let parseMemoryWrite (input : string) =
        (
            input.Substring(4, (input.IndexOf ']') - 4) |> uint64,
            input.Substring(input.IndexOf '=' + 2) |> uint64
        )
        |> Instruction.WriteMemory

    let parse input =
        let parseLine (l : string) =
            match l.Chars 1 with
            | 'a' -> l.Substring 7 |> parseBitmask
            | _ -> l |> parseMemoryWrite
        input |> Array.map parseLine

    let setBitNToZero n value = value &&& ~~~(1UL <<< n)
    let setBitNToOne  n value = value ||| (1UL <<< n)
    let setBitNToMatchSource n src value =
        match src &&& (1UL <<< n) with
        | 0UL -> setBitNToZero n value
        | _   -> setBitNToOne  n value

    let applyBitmask mask n =
        let doSingleBit res (i, bi) =
            match bi with
            | Zero -> res |> setBitNToZero i
            | One -> res |> setBitNToOne i
            | X -> res
        mask |> Array.indexed |> Array.fold doSingleBit n

    let doSetMemory (addr, value) state =
        let maskedValue = value |> applyBitmask state.Mask
        { state with Memory = state.Memory |> Map.add addr maskedValue }

    let runProgram initialState instructions =
        let runStep state instruction =
            match instruction with
            | SetBitmask mask -> { state with Mask = mask }
            | WriteMemory (addr, value) -> state |> doSetMemory (addr, value)
        instructions |> Array.fold runStep initialState

    let getFloatingAddresses addr mask =
        let splitAtIndex index n =
            [| n |> setBitNToZero index; n |> setBitNToOne index |]
        let doSingleBit res (i, bi) =
            match bi with
            | Zero -> res |> Array.map (setBitNToMatchSource i addr)
            | One -> res |> Array.map (setBitNToOne i)
            | X -> res |> Array.collect (splitAtIndex i)
        mask |> Array.indexed |> Array.fold doSingleBit [| 0UL |]

    let runProgramV2 initialState instructions =
        let runStep state instruction =
            match instruction with
            | SetBitmask mask -> { state with Mask = mask }
            | WriteMemory (addr, value) ->
                let addresses = getFloatingAddresses addr state.Mask
                let newMemory = addresses |> Array.fold (fun mem a -> mem |> Map.add a value) state.Memory
                { state with Memory = newMemory }
        instructions |> Array.fold runStep initialState

    let sumOfValuesInMemory state = state.Memory |> Map.fold (fun sum _ value -> sum + value) 0UL

    let solve input =
        let instructions = input |> parse
        let initialState = { Mask = Array.empty; Memory = Map.empty }
        let partOneSolution = instructions |> runProgram initialState |> sumOfValuesInMemory
        let partTwoSolution = instructions |> runProgramV2 initialState |> sumOfValuesInMemory
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day14.solve
printfn "Day 14: [ %i, %i ]" (fst solution) (snd solution)
