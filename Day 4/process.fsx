// Day 3 challenge 
#load @"..\Read-data.fsx"
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 4\input-data.txt"

open System
open AoCUtils.Utils

type Slot =
    | Occupied
    | Empty

type RollMetadata = {SlotStatus : Slot; OccupiedNeighbours : int option}

let buildPaperRollArray (inputdata : string array) =
    inputdata
    |> Array.map (fun line -> 
            line.ToCharArray()
            |> Array.map (fun c -> 
                if c = '@' 
                then
                    {SlotStatus = Occupied; OccupiedNeighbours = None}
                else
                    {SlotStatus = Empty; OccupiedNeighbours = None}
                ))

let buildPaperRollMetadataArray paperRollArray =
    let updateFunc (row' : int) (col' : int) (neighbours : RollMetadata array array) metadata = 
        match metadata.SlotStatus with
        | Occupied -> 
            let mutable count = 0
            for r in 0 .. neighbours.Length - 1 do
                for c in 0 .. neighbours[0].Length - 1 do
                    let neighbour = neighbours[r][c]
                    match neighbour.SlotStatus with
                    | Occupied -> 
                        count <- count + 1
                    | Empty -> 
                        count <- count

            {SlotStatus = Occupied; OccupiedNeighbours = Some (count - 1)}

        | Empty -> {SlotStatus = Empty; OccupiedNeighbours = None}

    paperRollArray |> updateMatrixMetadataFromNeighbours updateFunc
    
let removeRolls rolls =
    let mutable removedCount = 0
    rolls
    |> Array.map (fun row -> 
            row 
            |> Array.map (fun elem -> 
                match elem.SlotStatus, elem.OccupiedNeighbours with
                | Empty, _ -> elem
                | Occupied, Some n when n < 4 -> 
                    removedCount <- removedCount + 1
                    {SlotStatus = Empty; OccupiedNeighbours = None}
                | Occupied, _ -> elem
                ))
    |> fun updatedRolls -> (removedCount, updatedRolls) 

let iterateRollRemoval singleIteration rolls =
    let rec removeLoop stopIterating removedCount removedTotal currentRolls =
        let (removedCount, updatedRolls) = 
            removeRolls currentRolls

        let currentRolls = updatedRolls |> buildPaperRollMetadataArray    
        if removedCount = 0 || stopIterating then
            removedTotal, currentRolls
        else
            removeLoop singleIteration removedCount (removedTotal + removedCount) updatedRolls

    removeLoop false 0 0 rolls

let start filePath = 
    filePath
    |> ReadData.readLines
    |> Array.ofSeq 
    |> buildPaperRollArray 
    |> buildPaperRollMetadataArray

let part1  = iterateRollRemoval true
let part2  = iterateRollRemoval false

#time
filePath |> start |> part1 |> fun p1 -> printfn "Part 1: Total accessible slots: %d" (fst p1)
#time
#time
filePath |> start |> part2 |> fun p2 -> printfn "Part 2: Total accessible slots: %d" (fst p2)
#time