// Day 1 challenge 
#load @"..\Read-data.fsx"

let filePath = @"..\Day 2\input-data.txt"

open System
open System.Numerics

// Active pattern to test for duplicates withing Strings i.e invalid IDs
// let (|InvalidId|_|) (s: string) =
//     if (s.Length % 2) = 1 
//     then 
//         None 
//     else 
//         let (l,r) = s.Substring(0, s.Length / 2), s.Substring(s.Length / 2)
//         if l = r then Some s else None


let partitionString s chunkSize =
    let rec partitionString' (s: string) init matched =
        printfn "s: %s, init: %s, matched: %b" s init matched
        if not matched then matched
        else
            match s.Length with
            | 0 -> matched
            | _ -> 
                let s' = s.[chunkSize..]
                let nextChunkMatch = s.[0..chunkSize - 1] = init
                partitionString' s' init nextChunkMatch
    partitionString' s s.[0..chunkSize - 1] true

    
let (|InvalidId|_|) (s: string) =
      if s.Length % 2 = 1 
      then 
          [s.Length / 3 .. -2 .. 0]
      else 
          // process as even
  
let parseInput (line : string)  =
    let (start, finish) = line.Split("-") |> (fun arr -> (bigint.Parse(arr.[0]), bigint.Parse(arr.[1])))
    [start .. finish]
    |> List.map (fun x -> x.ToString())
    |> List.filter (function InvalidId _ -> true | _ -> false)
    |> List.map bigint.Parse
    
let start filePath =
    let lines = ReadData.readLines filePath
    lines
    |> Seq.map parseInput
    |> Seq.toList
    |> Seq.concat
    |> Seq.sum

#time    
filePath |> start |> printfn "Sum of all valid IDs: %A"

#time