// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 7\input-data.txt"

open AoCUtils.Utils
open System 

type Beam = {PathCount : int; Path : int}
type BeamData = {Hits: int; Beams: Beam list}

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
            |> List.map (fun b -> b.Path)
            |> List.filter (fun path -> not (newHits |> List.contains path))
            |> List.insertManyAt 0 newHitList
            |> List.distinct
            |> List.sort
            |> List.map (fun bp -> {PathCount = 0; Path = bp} )

        {beamData with Hits = beamData.Hits + newHits.Length; Beams = newBeams}


let procP1 input =
    input
    |> Array.fold (fun beamData beamArray ->
            let paths = beamData.Beams |> List.map (fun b -> b.Path)
            let splitters = locateSplitters paths beamArray
            
            updateBeamData beamData splitters)
            {Hits = 0; Beams = []}

let inputData = 
    filePath
    |> ReadData.readLines
    |> Seq.toArray
    |> Seq.map (fun s -> s.ToCharArray())
    |> Seq.toArray

#time
//inputData |> procP1 |> (printfn "Part 1 result is s: %A") 
#time

#time
//part2result() |> (printfn "Part 2 result is s: %d") 
#time