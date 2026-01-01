// Day 3 challenge 
#load @"..\Read-data.fsx"
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 3\input-data.txt"

open System
open AoCUtils.Utils

let evaluate numCells (sequencedCells : (int * int) seq) =
    let rec evaluate' totalCells remainingCells nextIdx result =
        match remainingCells with
        | 0 -> 
            result |> List.rev
        | _ ->
            let cell =
                sequencedCells
                |> Seq.skip nextIdx 
                |> Seq.take ((sequencedCells |> Seq.length) - nextIdx - remainingCells + 1)
                |> Seq.maxBy (fun (v, i) -> v)
            evaluate' totalCells (remainingCells - 1) ((snd cell) + 1) (cell::result)
            
    evaluate' (sequencedCells |> Seq.length) numCells 0 []

let parseLine batteryCells (line:string) =
    let indexedVals =
        line.ToCharArray()
        |> Seq.mapi (fun i c -> Int32.Parse(c.ToString()), i)
    
    let result =
        evaluate batteryCells indexedVals
        |> Seq.mapi (fun idx (v, i) -> (v, batteryCells - idx - 1))
        |> Seq.fold (fun acc (v, pow) ->
            acc + (int64 v) * int64 (Math.Pow(10.0, float pow))) 0L
    result

let parseLine_part1 = parseLine 2
let parseLine_part2 = parseLine 12

let start (parser : string -> int64) (filePath : string) =
    let lines = 
        ReadData.readLines filePath 
        |> Seq.map (fun s   -> parser s)
        |> Seq.sum
    lines
    
#time    
filePath |> start parseLine_part1 |> printfn "Part 1: %d" 
filePath |> start parseLine_part2 |> printfn "Part 2: %d" 

#time

