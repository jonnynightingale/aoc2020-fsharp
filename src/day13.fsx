module Day13 =

    [<Struct>]
    type BusData = {
        EarliestTime : int
        BusIDs : int ValueOption array
    }

    let parse (input : string array) =
        let parseID id =
            match id with
            | "x" -> ValueNone
            | str -> str |> int |> ValueSome
        {
            EarliestTime = int input.[0];
            BusIDs = input.[1].Split ',' |> Array.map parseID
        }

    let findEarliestBus busData =
        let etmo = busData.EarliestTime - 1
        busData.BusIDs
        |> Array.filter ValueOption.isSome
        |> Array.map ((ValueOption.get) >> (fun n -> n, (n - 1) - (etmo % n)))
        |> Array.minBy snd

    let findEarliestTime busData =
        let findTime (incr, time) (offset, id) =
            let rec impl n =
                if (offset = 0UL) then id
                elif (id - (n % id)) = (offset % id) then n
                else impl (n + incr)
            incr * id, impl time
        busData.BusIDs
        |> Array.indexed
        |> Array.filter (snd >> ValueOption.isSome)
        |> Array.map (fun (i, oid) -> uint64 i, uint64 oid.Value)
        |> Array.fold findTime (1UL, 1UL)
        |> snd

    let solve input =
        let busData = input |> parse
        let partOne = busData |> findEarliestBus |> (fun (a, b) -> a * b)
        let partTwo = busData |> findEarliestTime
        string partOne, string partTwo
