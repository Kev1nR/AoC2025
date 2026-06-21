// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 10\input-data.txt"

open AoCUtils.Utils
open System

type LightsButtons = 
    {
        ExpectedLightState: uint
        LightButtons: uint array    
    }

let lightStateToBits (lightStateString : string) = 
    let binStr = 
        (lightStateString[1..(lightStateString.Length - 2)]) // extract light state as .s and #s
         .Replace(".", "0").Replace("#","1") // Convert to binary representation
    
    uint $"0b{binStr}"

let buttonStateToBits (lightMask: string) (buttonStateString : string) = 
    let buttonIndexes = 
        buttonStateString[1..(buttonStateString.Length - 2)]
         .Split(",")
        |> Array.map (int)

    let buttonState = 
        buttonIndexes
        |> Array.fold (fun (acc: string) i ->
            let maxIdx = lightMask.Length - 1 

            if i = 0 then
                "1" + acc[1..]
            elif i = maxIdx then
                acc[..(maxIdx - 1)] + "1"
            else
                acc[..(i-1)] + "1" + acc[(i+1)..]
            ) lightMask
    
    uint $"0b{buttonState}"

let generateLightsandButtons (input : string) =
    let splitInput = input.Split(" ")
    let lightState = input.Split(" ")[0] |> lightStateToBits
    let buttonStates = 
        splitInput
        |> Array.filter (fun bs -> bs.StartsWith("("))
        |> Array.map (fun bs -> 
            bs |> buttonStateToBits (new string('0', splitInput[0].Length - 2)))

    {
        ExpectedLightState = lightState
        LightButtons = buttonStates
    }

let getLeastButtonPresses targetStateUint buttonStates =
    let rec bfs lightStates stage found =
        match found with
        | Some buttonPresses -> buttonPresses
        | None ->
            let nextLightStates = 
                lightStates
                |> Seq.allPairs buttonStates
                |> Seq.map (fun (bs, ls) -> 
                        uint (bs ^^^ ls))

            let hit =
                nextLightStates |> Seq.contains targetStateUint

            bfs nextLightStates (stage + 1) (if hit then Some (stage + 1) else None)
            
    bfs [0u] 0 None

let part1 () =
    filePath
    |> ReadData.readLines
    |> Seq.map (fun line -> line |> generateLightsandButtons)
    |> Seq.map (fun lbs ->
        getLeastButtonPresses lbs.ExpectedLightState lbs.LightButtons
    )
    |> Seq.sum   
    
#time
part1 () |> printfn "Part 1 result : %d" 
#time 

#time
#time