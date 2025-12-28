// Day 1 challenge 
#load @"..\Read-data.fsx"

let filePath = @"..\Day 1\input-data.txt"

open System

type DialTurn = { Steps: int; Loops: int; CompOp: int -> int -> bool }

let parseInput (loopPassStrategy : int -> int) (line: string) : DialTurn =
    let directionChar = line.[0]
    let steps' = Int32.Parse(line.[1..])
    let loops = loopPassStrategy steps' // / 100
    let steps = steps' % 100
    let dialTurn =
        match directionChar with
        | 'L' -> { Steps = 100 - steps; Loops = loops; CompOp = (>) } // Adapt so that all turns are processed as "clockwise"
        | 'R' -> { Steps = steps; Loops = loops; CompOp = (<) }  
        | _ -> failwithf "Invalid direction character: %c" directionChar
    dialTurn

let part1 = parseInput (fun _ -> 0)
let part2 = parseInput (fun s -> s / 100)

let dialReadings parser filePath =
    let lines = ReadData.readLines filePath
    lines
    |> Seq.map parser
    |> Seq.toList
    
let rec processData pos zeroCount ignoreZeroPass dialReadings =  
  match dialReadings with
  | []    -> zeroCount
  | h::t  -> 
    let newPos = ((+) pos h.Steps) % 100
    let hit = if ignoreZeroPass 
              then 
                if newPos = 0 then 1 else 0
              else
                if newPos = 0 || (pos <> 0 && h.CompOp newPos pos) 
                then 
                    h.Loops + 1 
                else 
                    h.Loops 

    processData newPos (zeroCount + hit) ignoreZeroPass t    

#time    
filePath |> dialReadings part1 |> processData 50 0 true |> printfn "Number of times dial hits zero: %d"
filePath |> dialReadings part2 |> processData 50 0 false |> printfn "Number of times dial hits zero: %d"
#time