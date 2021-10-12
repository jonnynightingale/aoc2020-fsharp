module Day10 =

    let sortAndAddEndpoints joltages =
        let sorted = joltages |> Array.sort
        let deviceJoltage = 3 + Array.last sorted
        Array.concat [| [| 0 |]; sorted; [| deviceJoltage |] |]

    let solvePartOne joltages =
        let diffs = joltages |> Array.sort |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
        let numberOfOnes = diffs |> Array.filter ((=) 1) |> Array.length
        let numberOfThrees = diffs |> Array.filter ((=) 3) |> Array.length
        numberOfOnes * numberOfThrees

    let solvePartTwo joltages =
        let pathFindingFold n state =
            let inRange = state |> Array.filter (fun (m, _) -> m <= n + 3)
            let sum = inRange |> Array.sumBy snd
            inRange |> Array.append [| (n, sum) |] 

        Array.foldBack pathFindingFold joltages [| (Array.last joltages + 3, 1L) |]
        |> Array.head
        |> snd

    let solve (input : string array) =
        let joltagesSortedWithEndpoints = input |> Array.map int |> sortAndAddEndpoints
        let partOne = joltagesSortedWithEndpoints |> solvePartOne
        let partTwo = joltagesSortedWithEndpoints |> solvePartTwo
        string partOne, string partTwo
