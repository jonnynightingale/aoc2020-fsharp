module Day17 =

    type CubeState = Inactive | Active

    let private iterateCubeState activeNeighborCount cubeState =
        match cubeState, activeNeighborCount with
        | Active, n when n = 2 || n = 3 -> Active
        | Inactive, n when n = 3 -> Active
        | _ -> Inactive

    let private parseCube = function
        | '#' -> Active
        | _ -> Inactive

    // Adding missing functionality of Array4D
    module Array4DHelpers =
        let map mapping array =
            let len1 = Array4D.length1 array
            let len2 = Array4D.length2 array
            let len3 = Array4D.length3 array
            let len4 = Array4D.length4 array
            let res = (Array4D.zeroCreate len1 len2 len3 len4 : 'b[,,,])
            for i = 0 to len1 - 1 do 
                for j = 0 to len2 - 1 do 
                    for k = 0 to len3 - 1 do 
                        for l = 0 to len4 - 1 do 
                            res.[i,j,k,l] <- mapping array.[i,j,k,l]
            res

        let mapi mapping array =
            let len1 = Array4D.length1 array
            let len2 = Array4D.length2 array
            let len3 = Array4D.length3 array
            let len4 = Array4D.length4 array
            let res = (Array4D.zeroCreate len1 len2 len3 len4 : 'b[,,,])
            let f = OptimizedClosures.FSharpFunc<_,_,_,_,_,_>.Adapt(mapping)
            for i = 0 to len1 - 1 do 
                for j = 0 to len2 - 1 do 
                    for k = 0 to len3 - 1 do 
                        for l = 0 to len4 - 1 do 
                            res.[i,j,k,l] <- f.Invoke(i, j, k, l, array.[i,j,k,l])
            res

    module Grid3D =

        let private neighborOffsets =
            let offsets1D = [| -1; 0; 1 |]
            offsets1D
            |> Array.allPairs offsets1D
            |> Array.allPairs offsets1D
            |> Array.map (fun (a, (b, c)) -> a, b, c)
            |> Array.filter ((<>) (0, 0, 0))

        let inline private addOffset (x, y, z) (ox, oy, oz) = x + ox, y + oy, z + oz

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

        let rec iterateN n currentState =
            match n with
            | 0 -> currentState
            | _ -> currentState |> iterate |> iterateN (n - 1)

        let parse (input : string array) =
            let dimX = input.[0].Length + 2
            let dimY = input.Length + 2
            let dimZ = 3
            let coordToCubeState x y z =
                match (x, y, z) with
                | c when isOnBoundary dimX dimY dimZ c -> Inactive
                | _ -> (input.[y - 1].Chars (x - 1)) |> parseCube
            Array3D.init dimX dimY dimZ coordToCubeState

    module Grid4D =

        let private neighborOffsets =
            let offsets1D = [| -1; 0; 1 |]
            offsets1D
            |> Array.allPairs offsets1D
            |> Array.allPairs offsets1D
            |> Array.map (fun (a, (b, c)) -> a, b, c)
            |> Array.allPairs offsets1D
            |> Array.map (fun (a, (b, c, d)) -> a, b, c, d)
            |> Array.filter ((<>) (0, 0, 0, 0))

        let inline private addOffset (x, y, z, w) (ox, oy, oz, ow) = x + ox, y + oy, z + oz, w + ow

        let private isOnBoundary dimX dimY dimZ dimW (x, y, z, w) =
            (x = 0) || (y = 0) || (z = 0) || (w = 0) || (x = dimX - 1) || (y = dimY - 1) || (z = dimZ - 1) || (w = dimW - 1)

        let private getIsValidCoordinate worldState (x, y, z, w) =
            let dimX = Array4D.length1 worldState
            let dimY = Array4D.length2 worldState
            let dimZ = Array4D.length3 worldState
            let dimW = Array4D.length4 worldState
            (x >= 0) && (y >= 0) && (z >= 0) && (w >= 0) && (x < dimX) && (y < dimY) && (z < dimZ) && (w < dimW)

        let private getNumberOfActiveNeighbors worldState coord =
            neighborOffsets
            |> Array.map (addOffset coord)
            |> Array.filter (getIsValidCoordinate worldState)
            |> Array.filter (fun (x, y, z, w) -> (Array4D.get worldState x y z w) = Active)
            |> Array.length

        let private addPadding worldState =
            let dimX = Array4D.length1 worldState + 2
            let dimY = Array4D.length2 worldState + 2
            let dimZ = Array4D.length3 worldState + 2
            let dimW = Array4D.length4 worldState + 2
            Array4D.init dimX dimY dimZ dimW (fun x y z w ->
                match (x, y, z, w) with
                | (x, y, z, w) when (x, y, z, w) |> isOnBoundary dimX dimY dimZ dimW -> Inactive
                | (x, y, z, w) -> Array4D.get worldState (x - 1) (y - 1) (z - 1) (w - 1)
            )

        let private iterate currentState =
            currentState
            |> Array4DHelpers.mapi (fun x y z w cubeState -> (x, y, z, w) |> getNumberOfActiveNeighbors currentState, cubeState)
            |> Array4DHelpers.map (fun (activeNeighborCount, cubeState) -> iterateCubeState activeNeighborCount cubeState)
            |> addPadding

        let rec iterateN n currentState =
            match n with
            | 0 -> currentState
            | _ -> currentState |> iterate |> iterateN (n - 1)

        let parse (input : string array) =
            let dimX = input.[0].Length + 2
            let dimY = input.Length + 2
            let dimZ = 3
            let dimW = 3
            let coordToCubeState x y z w =
                match (x, y, z, w) with
                | c when isOnBoundary dimX dimY dimZ dimW c -> Inactive
                | _ -> (input.[y - 1].Chars (x - 1)) |> parseCube
            Array4D.init dimX dimY dimZ dimW coordToCubeState

    let private countActiveCubes worldState =
        worldState |> Seq.cast<CubeState> |> Seq.filter ((=) Active) |> Seq.length

    let solve input =
        let partOne = input |> Grid3D.parse |> Grid3D.iterateN 6 |> countActiveCubes
        let partTwo = input |> Grid4D.parse |> Grid4D.iterateN 6 |> countActiveCubes
        uint64 partOne, uint64 partTwo

let solution = fsi.CommandLineArgs.[1] |> System.IO.File.ReadAllLines |> Day17.solve
printfn "Day 17: [ %i, %i ]" (fst solution) (snd solution)
