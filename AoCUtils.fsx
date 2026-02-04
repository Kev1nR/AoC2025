module Utils =

    open System.Collections.Generic
    open System.IO
    open System.Numerics

    module ReadData =
        let readLines (filePath:string) = 
            seq {
                    use sr = new StreamReader (filePath)
                    while not sr.EndOfStream do
                        yield sr.ReadLine ()
                }    

    let memoize (f: _ -> _) =
        let cache = Dictionary<_, _>()
        fun x ->
            if cache.ContainsKey(x) then
                cache.[x]
            else
                let result = f x
                cache.[x] <- result
                result

    let buildMatrixMetadata 
        (initFunc : 'a -> 'b)
        arr =

        let rows = arr |> Array.length
        let cols = arr.[0] |> Array.length

        Array.init rows (fun r ->
            Array.init cols (fun c ->
                initFunc arr.[r].[c]
            )
        )
    
    let updateMatrixMetadata 
        (updateFunc : int -> int ->'a -> 'a)
        arr =

        let rows = arr |> Array.length
        let cols = arr.[0] |> Array.length

        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                arr.[r].[c] <- updateFunc r c arr.[r].[c]
        
        arr

    let buildNeighbours r c (arr' : 'a array array) = 
        [|
            let startRow = max 0 (r - 1)
            let endRow = min (arr'.Length - 1) (r + 1)
            let startCol = max 0 (c - 1)
            let endCol = min (arr'[0].Length - 1) (c + 1)

            for r' in startRow .. endRow do
                [|
                    for c' in startCol .. endCol do
                        arr'[r'][c']
                |]
       |]
    
    let updateMatrixMetadataFromNeighbours 
        (updateFunc : int -> int -> 'a array array ->'a -> 'a)
        arr =

        let rows = arr |> Array.length
        let cols = arr.[0] |> Array.length
        
        for r in 0 .. rows - 1 do
            for c in 0 .. cols - 1 do
                let neighbours = buildNeighbours r c arr

                arr.[r].[c] <- updateFunc r c neighbours arr.[r].[c]
        
        arr
    
    let transposeMatrix (matrix : 'a array array) =
        let allRowsEqualLength =
            matrix
            |> Array.forall (fun row -> row.Length = matrix.[0].Length)

        if not allRowsEqualLength then
            failwith "All rows in the matrix must be of equal length to transpose"

        [|
            for c in 0 .. matrix.[0].Length - 1 do
                [|
                    for r in 0 .. matrix.Length - 1 do
                        matrix.[r].[c]
                |]
        |] 

    let toUpperTriangle (input : 'a array) =
        [|
            for row in 0 .. input.Length - 1 do
                [|
                    for col in row .. input.Length - 1 do
                        input[col]
                |]
        |]

    let splitWhen splitValue input =  
        let rec proc' vals collectVals acc =
            match vals with
            | [] -> collectVals::acc 
                    |> List.map (fun l -> l |> List.rev |> List.toArray)
                    |> List.rev |> List.toArray
            | v::vs ->
                if v = splitValue
                then
                    proc' vs [] (collectVals::acc)
                else
                    proc' vs (v::collectVals) acc              

        proc' (input |> Array.toList) [] []         

        