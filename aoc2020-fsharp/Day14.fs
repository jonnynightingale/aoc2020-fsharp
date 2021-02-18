module Day14

type BitInstruction = SetZero | SetOne | Copy

type private Instruction =
    | SetBitmask of BitInstruction array
    | WriteMemory of int * uint64

[<Struct>]
type private ProgramState = {
    mask : BitInstruction array
    memory : Map<int, uint64>
}

let private ParseBitmask (input : string) =
    input.ToCharArray()
    |> Array.rev
    |> Array.map (fun c -> match c with '0' -> SetZero | '1' -> SetOne | _ -> Copy)
    |> Instruction.SetBitmask

let private ParseMemoryWrite (input : string) =
    (
        input.Substring(4, (input.IndexOf ']') - 4) |> int,
        input.Substring(input.IndexOf '=' + 2) |> uint64
    )
    |> Instruction.WriteMemory

let private Parse (input : string array) =
    let ParseLine (l : string) =
        match l.Chars 1 with
        | 'a' -> l.Substring 7 |> ParseBitmask
        | _ -> l |> ParseMemoryWrite
    input |> Array.map ParseLine

let ApplyBitmask (mask : BitInstruction array) (n : uint64) =
    let DoSingleBit res (i, bi) =
        match bi with
        | SetZero -> res &&& ~~~(1UL <<< i)
        | SetOne -> res ||| (1UL <<< i)
        | Copy -> res
    mask
    |> Array.indexed
    |> Array.fold DoSingleBit n

let private DoSetMemory ((addr, value) : int * uint64) (state : ProgramState) =
    let maskedValue = value |> ApplyBitmask state.mask
    { state with memory = state.memory |> Map.add addr maskedValue }

let private RunProgram initialState instructions =
    let RunStep (state : ProgramState) (instruction : Instruction) =
        match instruction with
        | SetBitmask mask -> { state with mask = mask }
        | WriteMemory (addr, value) -> state |> DoSetMemory (addr, value)
    instructions |> Array.fold RunStep initialState

let private SumOfValuesInMemory (state : ProgramState) =
    state.memory |> Map.fold (fun sum _ value -> sum + value) 0UL

let Solve input =
    let instructions = input |> Parse
    let initialState = { mask = Array.empty; memory = Map.empty }
    let partOneSolution = instructions |> RunProgram initialState |> SumOfValuesInMemory
    uint64 partOneSolution, uint64 0
