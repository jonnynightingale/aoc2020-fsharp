module Day22 =

    type Player = One | Two

    [<Struct>]
    type GameState = {
        Player1 : int list
        Player2 : int list
    }

    let determineWinner gameState = if gameState.Player1 |> List.isEmpty then Player.Two else Player.One

    let getPlayerScore gameState player =
        let getDeckScore = List.rev >> List.mapi (fun idx n -> (idx + 1) * n) >> List.sum
        match player with
        | One -> gameState.Player1 |> getDeckScore
        | Two -> gameState.Player2 |> getDeckScore

    let playCombatSingleTurn initialState =
        let p1, p2 = initialState.Player1, initialState.Player2
        match p1, p2 with
        | h1 :: t1, h2 :: t2 ->
            if h1 > h2 then
                { Player1 = t1 @ [h1; h2]; Player2 = t2 }
            else
                { Player1 = t1; Player2 = t2 @ [h2; h1] }
        | _ -> initialState

    let rec playCombatToCompletion initialState =
        let p1, p2 = initialState.Player1, initialState.Player2
        match (p1, p2) with
        | [], _ | _, [] -> initialState
        | _ -> initialState |> playCombatSingleTurn |> playCombatToCompletion

    let playRecursiveCombatToCompletion initialState =
        let rec impl previousSeenStates initialState =
            if previousSeenStates |> List.contains initialState then
                initialState
            else
                let p1, p2 = initialState.Player1, initialState.Player2
                match (p1, p2) with
                | [], _ | _, [] -> initialState
                | h1 :: t1, h2 :: t2 when (t1 |> List.length |> (<=) h1) && (t2 |> List.length |> (<=) h2) ->
                    let innerGameState = {
                        Player1 = t1 |> List.take h1
                        Player2 = t2 |> List.take h2
                    }
                    let innerGameWinner = innerGameState |> impl [] |> determineWinner
                    match innerGameWinner with
                    | One -> { Player1 = t1 @ [h1; h2]; Player2 = t2 } |> impl (initialState :: previousSeenStates)
                    | Two -> { Player1 = t1; Player2 = t2 @ [h2; h1] } |> impl (initialState :: previousSeenStates)
                | _ -> initialState |> playCombatSingleTurn |> impl (initialState :: previousSeenStates)

        initialState |> impl []

    let getWinnerScore gameState =
        let winner = gameState |> determineWinner
        getPlayerScore gameState winner

    let parse input =
        let blankIndex = input |> Array.findIndex (String.length >> (=) 0)
        {
            Player1 = input.[1 .. blankIndex - 1] |> Array.map int |> Array.toList
            Player2 = input.[blankIndex + 2 ..] |> Array.map int |> Array.toList
        }

    let solve input =
        let initialState = input |> parse
        let partOne = initialState |> playCombatToCompletion |> getWinnerScore
        let partTwo = initialState |> playRecursiveCombatToCompletion |> getWinnerScore
        string partOne, string partTwo
