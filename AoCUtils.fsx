module Utils =

    open System.Collections.Generic
    open System
    
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
    