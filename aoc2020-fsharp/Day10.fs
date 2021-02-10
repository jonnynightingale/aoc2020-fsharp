module Day10

let private SortAndAddEndpoints joltages =
    let sorted = joltages |> Array.sort
    let deviceJoltage = 3 + Array.last sorted
    Array.concat [| [| 0 |]; sorted; [| deviceJoltage |] |]

let private SolvePartOne joltages =
    let diffs = joltages |> Array.sort |> Array.pairwise |> Array.map (fun (a, b) -> b - a)
    let numberOfOnes   = diffs |> Array.filter ((=) 1) |> Array.length
    let numberOfThrees = diffs |> Array.filter ((=) 3) |> Array.length
    numberOfOnes * numberOfThrees

let private SolvePartTwo (joltages : int array) =
    let PathFindingFold (n : int) (state : (int * int64) array) =
        let inRange = state |> Array.filter (fun (m, _) -> m <= n + 3)
        let sum = inRange |> Array.map snd |> Array.sum
        inRange |> Array.append [| (n, sum) |] 
    Array.foldBack PathFindingFold joltages [| (Array.last joltages + 3, 1L) |]
    |> Array.head
    |> snd

let Solve (input : string array) =
    let joltagesSortedWithEndpoints = input |> Array.map int |> SortAndAddEndpoints
    let partOneSolution = joltagesSortedWithEndpoints |> SolvePartOne
    let partTwoSolution = joltagesSortedWithEndpoints |> SolvePartTwo
    uint64 partOneSolution, uint64 partTwoSolution
