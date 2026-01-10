#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 4\process.fsx"

open Expecto
open Process

let inputdata = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."

// Example function to test

 
// Define tests
let tests =
    testList "Part 1 tests" [
        testCase "Can build an array of initialised metadatafrom input data" <| fun _ ->
            let expected = {SlotStatus = Empty; OccupiedNeighbours = None}
            
            let result = buildPaperRollArray inputdata
            
            Expect.equal result.Length 10 "Result array should have 10 rows"
            Expect.equal (result[0][0]) expected "Empty element expected"
        
        testCase "Third element matches input data with attached metadata" <| fun _ ->    
            let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 3}

            let result = 
                buildPaperRollArray inputdata
                |> buildPaperRollMetadataArray

            Expect.equal (result[0][2]) expected "Occupied element with 3 neighbours expected"

        testCase "5,7 element matches input data with attached metadata" <| fun _ ->    
            let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 5}

            let result = 
                buildPaperRollArray inputdata
                |> buildPaperRollMetadataArray

            Expect.equal (result[5][7]) expected "Occupied element with 5 neighbours expected"

        testCase "All test data should get 13 result" <| fun _ ->    
            let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 5}

            let result = 
                buildPaperRollArray inputdata
                |> buildPaperRollMetadataArray

            let totalAccessible = 
                result
                |> Array.sumBy (fun row -> 
                    row 
                    |> Array.fold (fun acc elem -> 
                                    printfn "Element %A" elem
                                    match elem.OccupiedNeighbours with
                                    | Some n when n < 4 -> acc + 1
                                    | _ -> acc
                                ) 0
                )

            Expect.equal totalAccessible 13 "Total accessible elements should be 13"
    ] 
    
let main argv =
    runTestsWithArgs defaultConfig argv tests

main [||]