module Day22 =

    [<Struct>]
    type GameState = {
        Player1 : int list
        Player2 : int list
    }

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

    let getWinnerScore gameState =
        let getDeckScore = List.rev >> List.mapi (fun idx n -> (idx + 1) * n) >> List.sum
        let player1Score = gameState.Player1 |> getDeckScore
        let player2Score = gameState.Player2 |> getDeckScore
        max player1Score player2Score

    let parse input =
        let blankIndex = input |> Array.findIndex (String.length >> (=) 0)
        {
            Player1 = input.[1 .. blankIndex - 1] |> Array.map int |> Array.toList
            Player2 = input.[blankIndex + 2 ..] |> Array.map int |> Array.toList
        }

    let solve input =
        let initialState = input |> parse
        let partOne = initialState |> playCombatToCompletion |> getWinnerScore
        string partOne, ""
