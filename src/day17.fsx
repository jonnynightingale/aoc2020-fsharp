module Day17 =

    type private CubeState = Inactive | Active

    let private neighborOffsets1D = [| -1; 0; 1 |]
    
    let private neighborOffsets2D = neighborOffsets1D |> Array.allPairs neighborOffsets1D

    let private neighborOffsets3D =
        neighborOffsets2D
        |> Array.allPairs neighborOffsets1D
        |> Array.map (fun (a, (b, c)) -> a, b, c)

    let inline private addOffset ((cx, cy, cz) : int * int * int) ((ox, oy, oz) : int * int * int) =
        cx + ox, cy + oy, cz + oz

    let private neighborOffsets = [|
        (-1, -1, -1); (-1, -1,  0); (-1, -1,  1);
        (-1,  0, -1); (-1,  0,  0); (-1,  0,  1);
        (-1,  1, -1); (-1,  1,  0); (-1,  1,  1);

        ( 0, -1, -1); ( 0, -1,  0); ( 0, -1,  1);
        ( 0,  0, -1);               ( 0,  0,  1);
        ( 0,  1, -1); ( 0,  1,  0); ( 0,  1,  1);

        ( 1, -1, -1); ( 1, -1,  0); ( 1, -1,  1);
        ( 1,  0, -1); ( 1,  0,  0); ( 1,  0,  1);
        ( 1,  1, -1); ( 1,  1,  0); ( 1,  1,  1);
    |]

    let private isOnBoundary dimX dimY dimZ (x, y, z) =
        (x = 0) || (y = 0) || (z = 0) || (x = dimX - 1) || (y = dimY - 1) || (z = dimZ - 1)

    let private getIsValidCoordinate worldState (x, y, z) =
        let dimX = Array3D.length1 worldState
        let dimY = Array3D.length2 worldState
        let dimZ = Array3D.length3 worldState
        (x >= 0) && (y >= 0) && (z >= 0) && (x < dimX) && (y < dimY) && (z < dimZ) 

    let private getNumberOfActiveNeighbors worldState coord =
        neighborOffsets
        |> Array.map (addOffset coord)
        |> Array.filter (getIsValidCoordinate worldState)
        |> Array.filter (fun (x, y, z) -> (Array3D.get worldState x y z) = Active)
        |> Array.length

    let private iterateCubeState activeNeighborCount cubeState =
        match cubeState, activeNeighborCount with
        | Active, n when n = 2 || n = 3 -> Active
        | Inactive, n when n = 3 -> Active
        | _ -> Inactive

    let private addPadding worldState =
        let dimX = Array3D.length1 worldState + 2
        let dimY = Array3D.length2 worldState + 2
        let dimZ = Array3D.length3 worldState + 2
        Array3D.init dimX dimY dimZ (fun x y z ->
            match (x, y, z) with
            | (x, y, z) when (x, y, z) |> isOnBoundary dimX dimY dimZ -> Inactive
            | (x, y, z) -> Array3D.get worldState (x - 1) (y - 1) (z - 1)
        )

    let private iterate currentState =
        currentState
        |> Array3D.mapi (fun x y z cubeState -> (x, y, z) |> getNumberOfActiveNeighbors currentState, cubeState)
        |> Array3D.map (fun (activeNeighborCount, cubeState) -> iterateCubeState activeNeighborCount cubeState)
        |> addPadding

    let rec private iterateN n currentState =
        match n with
        | 0 -> currentState
        | _ -> currentState |> iterate |> iterateN (n - 1)

    let private parseCube = function
        | '#' -> Active
        | _ -> Inactive

    let private parse (input : string array) =
        let dimX, dimY, dimZ = input.[0].Length + 2, input.Length + 2, 3
        let coordToCubeState x y z =
            match (x, y, z) with
            | c when isOnBoundary dimX dimY dimZ c -> Inactive
            | _ -> (input.[y - 1].Chars (x - 1)) |> parseCube
        Array3D.init dimX dimY dimZ coordToCubeState

    let private countActiveCubes worldState =
        worldState |> Seq.cast<CubeState> |> Seq.filter ((=) Active) |> Seq.length

    let solve input =
        let initialState = parse input
        let partOne = initialState |> iterateN 6 |> countActiveCubes
        uint64 partOne, uint64 0

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day17.solve
printfn "Day 17: [ %i, %i ]" (fst solution) (snd solution)
