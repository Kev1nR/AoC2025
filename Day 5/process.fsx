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

let result = 
    filePath 
    |> ReadData.readLines 
    |> parseData 
    ||> checkFreshness 
    |> List.length

printfn "Number of fresh ingredients: %d" result