// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 8\input-data.txt"

open AoCUtils.Utils
open System 
open System.Numerics

type IndexedVector = {Index: int; Vector : Vector3}
type IndexedDistances = {FromIndex: int; ToIndex: int; Distance : float32}
type Circuit = int list

let calcDistances input =
    let inputLen = input |> Array.length
    seq {
        for i in 0 .. inputLen - 1 do
            for j in (i + 1) .. inputLen - 1 do
                let distance = Vector3.Distance (input[i].Vector, input[j].Vector)
                {FromIndex = i; ToIndex = j; Distance = distance}
    }

let sortedDistances input =
    input
    |> Array.mapi (fun idx [|(x : string) ;y;z|] -> {Index = idx; Vector = Vector3(float32 x, float32 y, float32 z)})
    |> calcDistances
    |> Seq.sortBy (fun ds -> ds.Distance)
    |> Seq.toArray

let buildCircuits input =
    let rec build distances (circuits : Circuit list) =
        match distances with
        | [] -> circuits
        | h::t -> 
            let found =
                circuits
                |> List.filter (fun (c : Circuit) -> 
                    c |> List.exists (fun c' -> c' = h.FromIndex || c' = h.ToIndex))

            match found with
            | [] -> 
                let newCircuit = [h.FromIndex; h.ToIndex] |> List.sort
                build t (newCircuit::circuits)
            | f::[] -> 
                // Found in one circuit so append it                
                let newCircuit = 
                    h.FromIndex::h.ToIndex::f |> List.distinct |> List.sort

                let foundIndex = circuits |> List.findIndex (fun c -> c = f)

                let newCircuits =
                    circuits 
                    |> List.removeAt foundIndex
                    |> List.insertAt 0 newCircuit
                    
                build t newCircuits
            | f::g::[] ->
                // Found in two circuits so merge them 
                let newCircuit = 
                    found |> List.concat |> List.distinct |> List.sort

                let foundIndexF = circuits |> List.findIndex (fun c -> c = f)
                
                let newCircuits =
                    circuits 
                    |> List.removeAt foundIndexF
                
                let foundIndexG = newCircuits |> List.findIndex (fun c -> c = g)
                                    
                let newCircuits =
                    newCircuits 
                    |> List.removeAt foundIndexG
                    |> List.insertAt 0 newCircuit
            
                build t newCircuits    
                
    build (input |> Array.toList) []

let proc1 input =
    input
    |> buildCircuits
    |> List.sortByDescending (fun c -> c |> List.length)
    |> List.take 3
    |> List.map (fun item -> item.Length)
    |> List.fold (*) 1

let inputData = 
    filePath
    |> ReadData.readLines
    |> Seq.toArray
    |> Array.map (fun triplet ->
        triplet.Split(','))
    
#time
inputData 
|> sortedDistances 
|> Array.take 1000
|> proc1
|> printfn "Part 1 result: %d"
#time 