// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 5\input-data.txt"

open AoCUtils.Utils

type Range = { Start: int64; End: int64 }

let parseData (lines: string seq)  =
    lines
    |> Seq.fold (fun (freshIngredientRanges, ingredientRanges) line ->
        if line.Contains("-") then
            let parts = line.Split('-')
            let range = { Start = int64 parts[0]; End = int64 parts[1] }
            (range :: freshIngredientRanges, ingredientRanges)

        elif line.Trim() = "" then
            freshIngredientRanges, ingredientRanges
        else
            freshIngredientRanges, (int64 line :: ingredientRanges)
    ) ([],[])

let checkFreshness freshRanges ingredients =
    ingredients
    |> List.filter (fun ingredient ->
        freshRanges
        |> List.exists (fun range ->
            ingredient >= range.Start && ingredient <= range.End
        )
    )

let squash (ranges: Range list) =
    let rec squash' currentRange accumulatedRanges remainingRanges =
        match remainingRanges with
        | [] -> 
            accumulatedRanges
        | h::[] ->
            if h.Start <= currentRange.End then
                let newCurrentRange = { Start = currentRange.Start; End = max currentRange.End h.End }
                squash' newCurrentRange (newCurrentRange::accumulatedRanges) []        
            else
                squash' h (h::currentRange::accumulatedRanges) []
        | h::t -> 
            if h.Start <= currentRange.End then
                let newCurrentRange = { Start = currentRange.Start; End = max currentRange.End h.End }
                squash' newCurrentRange accumulatedRanges t        
            else
                squash' h (currentRange::accumulatedRanges) t


    let firstRange,rest = 
        match ranges |> List.sortBy (fun r -> r.Start) with
        | h::t -> h, t
        | [] -> failwith "No ranges to squash"

    squash' firstRange [] rest    

let part1result() = 
    filePath 
    |> ReadData.readLines 
    |> parseData 
    ||> checkFreshness 
    |> List.length

let part2result() =
    filePath 
    |> ReadData.readLines 
    |> parseData 
    |> fun (freshRanges, _) -> 
        freshRanges
        |> squash
        |> List.sumBy (fun range -> range.End - range.Start + 1L)

#time
part1result() |> (printfn "Number of fresh ingredients: %d") 
#time

#time
part2result() |> (printfn "All fresh ingredients: %d") 
#time