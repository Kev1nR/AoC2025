// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 6\input-data.txt"

open AoCUtils.Utils
open System 

let splitOperatorsFromValues inputData = 
    let operators, values =
        inputData
        |> fun arr ->
            let operators = arr |> Array.last |> fun (ops : string) -> ops.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            let numbers = arr |> Array.take (arr.Length - 1)
            (operators, numbers)
    (operators, values)

let mapToLongAndReduce (operators : string array) (values : string array array) =
        values
        |> Array.map (fun sarr -> 
            sarr 
            |> Array.map int64)
        |> Array.mapi (fun idx nums -> 
            match operators[idx] with
            | "*" -> 
                nums |> Array.reduce (fun acc next -> acc * next)
            | "+" -> 
                nums |> Array.reduce (fun acc next -> acc + next)
            | _ -> failwithf "Unrecognised operator %s" (operators[idx]))
        |> Array.sum

let processP2 inputData =
    let operators, values = splitOperatorsFromValues inputData
    
    let transposed = 
        values
        |> Array.map (fun (line : string) -> line.ToCharArray())
        |> Array.transpose

    let squashed = 
        transposed
        |> Array.fold (fun acc next -> 
                let n' = 
                    next 
                    |> Array.fold (fun acc' v -> sprintf "%s%c" acc' v) "" 
                    |> fun s -> s.Trim()

                n' :: acc ) []
        |> List.toArray
        |> Array.rev
        |> splitWhen "" 
    
    let result = mapToLongAndReduce operators squashed

    result
    
let processP1 inputData =
    let operators, values =
        inputData
        |> fun arr ->
            let operators = arr |> Array.last |> fun (ops : string) -> ops.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            let numbers = arr |> Array.take (arr.Length - 1)
            (operators, numbers)

    let itemiseValues =
        values 
        |> Array.map (fun vs -> vs.Split (' ', StringSplitOptions.RemoveEmptyEntries))
        |> Array.transpose

    let result = mapToLongAndReduce operators itemiseValues

    result
    
let part1result() = 
    filePath  
    |> ReadData.readLines
    |> Seq.toArray
    |> processP1

let part2result() =
    filePath 
    |> ReadData.readLines
    |> Seq.toArray
    |> processP2

#time
part1result() |> (printfn "Part 1 result is s: %d") 
#time

#time
part2result() |> (printfn "Part 2 result is s: %d") 
#time