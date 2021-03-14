module Day13 =

    [<Struct>]
    type private BusData = {
        earliestTime : int
        busIds : int ValueOption array
    }

    let private Parse (input : string array) =
        let ParseId id =
            match id with
            | "x" -> ValueNone
            | str -> str |> int |> ValueSome
        {
            earliestTime = int input.[0];
            busIds = input.[1].Split ',' |> Array.map ParseId
        }

    let private FindEarliestBus busData =
        let etmo = busData.earliestTime - 1
        busData.busIds
        |> Array.filter ValueOption.isSome
        |> Array.map ValueOption.get
        |> Array.map (fun n -> n, (n - 1) - (etmo % n))
        |> Array.minBy snd

    let private FindEarliestTime busData =
        let FindTime (incr, time) (offset, id) =
            let rec Impl n =
                if (offset = 0UL) then id
                elif (id - (n % id)) = (offset % id) then n
                else Impl (n + incr)
            incr * id, Impl time
        busData.busIds
        |> Array.indexed
        |> Array.filter (snd >> ValueOption.isSome)
        |> Array.map (fun (i, oid) -> uint64 i, uint64 oid.Value)
        |> Array.fold FindTime (1UL, 1UL)
        |> snd

    let Solve input =
        let busData = input |> Parse
        let partOneSolution = busData |> FindEarliestBus |> (fun (a, b) -> a * b)
        let partTwoSolution = busData |> FindEarliestTime
        uint64 partOneSolution, uint64 partTwoSolution

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day13.Solve
printfn "Day 13: [ %i, %i ]" (fst solution) (snd solution)
