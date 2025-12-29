module Utils =

    open System.Collections.Generic
    
    let memoize (f: _ -> _) =
        let cache = Dictionary<_, _>()
        fun x ->
            if cache.ContainsKey(x) then
                cache.[x]
            else
                let result = f x
                cache.[x] <- result
                result

