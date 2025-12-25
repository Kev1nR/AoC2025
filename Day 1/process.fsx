// Day 1 challenge 
#load @"..\Read-data.fsx"

let filePath = @"..\Day 1\input-data.txt"

open System

type DialTurn = { Steps: int; Loops: int; CompOp: int -> int -> bool }

let parseInput (line: string) : DialTurn =
    let directionChar = line.[0]
    let steps' = Int32.Parse(line.[1..])
    let loops = steps' / 100
    let steps = steps' % 100
    let dialTurn =
        match directionChar with
        | 'L' -> { Steps = 100 - steps; Loops = loops; CompOp = (>) } // Adapt so that all turns are processed as "clockwise"
        | 'R' -> { Steps = steps; Loops = loops; CompOp = (<) }  
        | _ -> failwithf "Invalid direction character: %c" directionChar
    dialTurn

let dialReadings filePath =
    let lines = ReadData.readLines filePath
    lines
    |> Seq.map parseInput
    |> Seq.toList
    
let rec processData pos zeroCount dialReadings =  
  match dialReadings with
  | []    -> zeroCount
  | h::t  -> 
    let newPos = ((+) pos h.Steps) % 100
    let hit = if newPos = 0 || (pos <> 0 && h.CompOp newPos pos) then h.Loops + 1 else h.Loops

    processData newPos (zeroCount + hit) t    

#time    
filePath |> dialReadings |> processData 50 0 |> printfn "Number of times dial hits zero: %d"
#time