// Day 1 challenge 
#load @"..\Read-data.fsx"

let filePath = @"..\Day 2\input-data.txt"

open System
open System.Numerics
open System.Collections.Generic
 
let partitionString s chunkSize =
    let rec partitionString' (s: string) init matched =
        // printfn "s: %s, init: %s, matched: %b" s init matched
        if not matched then matched
        else
            match s.Length with
            | 0 -> matched
            | _ -> 
                let s' = s.[chunkSize..]
                let nextChunkMatch = s.[0..chunkSize - 1] = init
                partitionString' s' init nextChunkMatch
    partitionString' s s.[0..chunkSize - 1] true

let memoize (f: _ -> _) =
    let cache = Dictionary<_, _>()
    fun x ->
        if cache.ContainsKey(x) then
            cache.[x]
        else
            let result = f x
            cache.[x] <- result
            result

let divisors n =
    seq { for i in 1 .. int (Math.Sqrt(float n)) do
            if n % i = 0 then
                yield i
                if i <> n / i && i > 1 then
                    yield n / i }
    |> Seq.sort
    |> Seq.toList

let memoDiv = memoize divisors

let testDivisors () =
    let testCases = 
        [ 
            (16, [1;2;4;8;16]); 
            (15, [1;3;5;15]); 
            (28, [1;2;4;7;14;28]); 
            (16, [1;2;4;8;16])
        ]

    testCases
    |> List.iter (fun (input, expected) ->
        let result = memoDiv input
        printfn "Divisors of %d: %A" input result
        if result <> expected then
            printfn "Test failed for input %d: expected %A but got %A" input expected result
        else
            printfn "Test passed for input %d" input
    )

let rec chunkString (s : string) chunkSize chunks  =
    match s.Length with
    | 0 -> List.rev chunks
    | _ -> 
        let chunk = s.[0..chunkSize - 1]
        let s' = s.[chunkSize..]
        chunkString s' chunkSize (chunk :: chunks)

let testChunkString () =
    let testCases = [ ("aabbcc", 2, ["aa"; "bb"; "cc"]); ("abcdef", 3, ["abc"; "def"]); ("aaaaaa", 1, ["a"; "a"; "a"; "a"; "a"; "a"]); ("", 2, [])]
    testCases
    |> List.iter (fun (input, chunkSize, expected) ->
        let result = chunkString input chunkSize []
        if result <> expected then
            printfn "Test failed for input %s with chunk size %d: expected %A but got %A" input chunkSize expected result
        else
            printfn "Test passed for input %s with chunk size %d" input chunkSize
    )   

let allMatch lst =
    match lst with
    | [] -> false
    | h::t -> List.fold (fun acc elem -> acc && (h = elem)) true t

let (|InvalidId|_|) (s: string) =
    let checkstring = 
        if s.Length = 1 then []
        else
            memoDiv s.Length
            |> List.filter (fun d -> 
                                chunkString s d []
                                |> allMatch)
    
    match checkstring with
    | [] -> None
    | _  -> Some s
      
let testInvalidIDs () =
    let testCases = 
        [ 
            ("aabbcc", None); 
            ("abcabc", Some "abcabc"); 
            ("aaaaaa", Some "aaaaaa"); 
            ("", None); 
            ("abcabc", None)
        ]

    testCases
    |> List.iter (fun (input, expected) ->
        let result = 
            match input with
            | InvalidId id -> Some id
            | _ -> None
        if result <> expected then
            printfn "Test failed for input %s: expected %A but got %A" input expected result
        else
            printfn "Test passed for input %s" input
    )   

let testChunker () =
    let testCases = [ ("aabbcc", 2, false); ("abcabc", 3, true); ("aaaaaa", 1, true); ("", 2, false); ("abcabc", 2, false)]
    testCases
    |> List.iter (fun (input, chunkSize, expected) ->
        let result = chunkString input chunkSize []
        printfn "Result: %A" result
        let x =
            result
            |> allMatch

        if x <> expected then
            printfn "Test failed for input %s with chunk size %d: expected %A but got %A" input chunkSize expected result
        else
            printfn "Test passed for input %s with chunk size %d" input chunkSize
    )

let parseInput (line : string)  =
    let (start, finish) = line.Split("-") |> (fun arr -> (bigint.Parse(arr.[0]), bigint.Parse(arr.[1])))
    [start .. finish]
    |> List.map (fun x -> x.ToString())
    |> List.filter (function InvalidId _ -> true | _ -> false)
    |> List.map bigint.Parse
    
let start filePath =
    let lines = 
        ReadData.readLines filePath 
                  |> Seq.head 
                  |> fun s -> s.Split(",")
    lines
    |> Seq.map (fun line -> parseInput line)
    |> Seq.toList
    |> Seq.concat
    |> Seq.sum

#time    
filePath |> start |> printfn "Sum of all valid IDs: %A"

#time

