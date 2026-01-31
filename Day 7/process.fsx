// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 7\input-data.txt"

open AoCUtils.Utils
open System 

type Beam = {PathCount : int64; Path : int}
type BeamData = {Hits: int; Beams: Beam list}

let locateSplitters (beams : Beam list) (input : char array) =
    let rec locate beams' splits =
        match beams' with
        | []    -> splits
        | h::t  -> 
            if input[h.Path] = '^' 
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
        | Some index -> [{PathCount = 1; Path = index}]
        | None -> failwithf "Beam could not be initiated"
    | _ ->
        locate beams []

let updateBeamData beamData newHits =
    match newHits, beamData.Beams with
    | [], _ -> beamData
    | _, [] -> // initiator beam
        let beams = newHits |> List.map (fun bp -> {PathCount = 1; Path = bp})
        {beamData with Beams = beams}
    | _   ->
        let newHitList = 
            newHits
            |> List.fold (fun acc n -> 
                let parent = 
                  beamData.Beams 
                  |> List.find (fun b -> b.Path = n)
                {PathCount = parent.PathCount; Path = (n-1)}
                  ::{PathCount = parent.PathCount; Path = (n+1)}::acc) []

        let newBeams =
            beamData.Beams
            |> List.filter (fun beam -> not (newHits |> List.contains beam.Path))
            |> List.insertManyAt 0 newHitList
            |> List.sortBy (fun b -> b.Path)
            |> List.fold (fun acc next -> 
                    match acc with
                    | [] -> 
                        let ret = next::acc
                        next::acc
                    | h::t when h.Path = next.Path ->
                        let ret = {h with PathCount = h.PathCount + next.PathCount} :: t
                        ret
                    | h::t -> 
                        let ret = next::h::t
                        ret
                        ) []
            |> List.sortBy (fun b -> b.Path)    
        
        {beamData with Hits = beamData.Hits + newHits.Length; Beams = newBeams}


let processData input =
    input
    |> Array.fold (fun beamData beamArray ->
            let splitters = 
                locateSplitters beamData.Beams beamArray
                |> List.map (fun b -> b.Path)

            updateBeamData beamData splitters )
            {Hits = 0; Beams = []}

let inputData = 
    filePath
    |> ReadData.readLines
    |> Seq.toArray
    |> Seq.map (fun s -> s.ToCharArray())
    |> Seq.toArray

#time
let prcRes = inputData |> processData 
prcRes |> fun res -> printfn "Part 1 result is: %d" res.Hits
#time

#time
prcRes |> fun res -> printfn "Part 2 result is: %d" (res.Beams |> List.sumBy (fun b -> b.PathCount))
#time