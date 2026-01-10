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


let buildPaperRollArray (input: string) =
    input.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
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
    