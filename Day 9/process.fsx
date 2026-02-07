// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 9\input-data.txt"

open AoCUtils.Utils
open System

let inputData = 
    filePath
    |> ReadData.readLines
    |> Seq.map (fun s -> 
        s.Split(','))
    |> Seq.map (fun [|a; b|]  -> (int64 a, int64 b)) 
    |> Seq.toArray
    
let calcAreas a b = 
    let a1, a2 = a
    let b1, b2 = b
    
    (a, b, (Int64.Abs(b1 - a1) + 1L) * (Int64.Abs(b2 - a2) + 1L))

#time
inputData 
|> pairwiseFold calcAreas
|> Seq.maxBy (fun (_,_, A) -> A)
|> printfn "%A"
#time 

#time
#time