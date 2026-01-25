// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 7\input-data.txt"

open AoCUtils.Utils
open System 

type BeamData = {Hits: int; Beams: int list}

let locateSplitters (beams : int list) (input : char array) =
    let rec locate beams' splits =
        match beams' with
        | []    -> splits
        | h::t  -> 
            if input[h] = '^' 
            then 
                locate t (h::splits)
            else
                locate t splits

    match beams with
    | [] -> 
        let beamIndex = 
            input 
            |> Array.tryFindIndex(fun c -> c = 'S')
        match beamIndex with
        | Some index -> [index]
        | None -> failwithf "Beam could not be initiated"
    | _ ->
        locate beams []

let updateBeamData beamData newHits =
    match newHits with
    | []  -> beamData
    | _   ->
        let newHitList = 
            newHits
            |> List.fold (fun acc n -> (n-1)::(n+1)::acc) []
        
        let newBeams =
            beamData.Beams 
            |> List.filter (fun path -> not (newHits |> List.contains path))
            |> List.insertManyAt 0 newHitList
            |> List.distinct
            |> List.sort

        {beamData with Hits = beamData.Hits + newHits.Length; Beams = newBeams}

//     filePath  
//     |> ReadData.readLines
//     |> Seq.toArray
//     |> processP1

// let part2result() =
//     filePath 
//     |> ReadData.readLines
//     |> Seq.toArray
//     |> processP2

#time
//part1result() |> (printfn "Part 1 result is s: %d") 
#time

#time
//part2result() |> (printfn "Part 2 result is s: %d") 
#time