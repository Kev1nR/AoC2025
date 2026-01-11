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
    testList "Day 4 tests" [
        testList "Part 1 tests" [
            testCase "Can build an array of initialised metadatafrom input data" <| fun _ ->
                let expected = {SlotStatus = Empty; OccupiedNeighbours = None}

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                
                Expect.equal result.Length 10 "Result array should have 10 rows"
                Expect.equal (result[0][0]) expected "Empty element expected"
            
            testCase "Third element matches input data with attached metadata" <| fun _ ->    
                let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 3}

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                    |> buildPaperRollMetadataArray

                Expect.equal (result[0][2]) expected "Occupied element with 3 neighbours expected"

            testCase "5,7 element matches input data with attached metadata" <| fun _ ->    
                let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 5}

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                    |> buildPaperRollMetadataArray

                Expect.equal (result[5][7]) expected "Occupied element with 5 neighbours expected"

            testCase "All test data should get 13 result" <| fun _ ->    
                let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 5}

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                    |> buildPaperRollMetadataArray

                let totalAccessible = 
                    result
                    |> Array.sumBy (fun row -> 
                        row 
                        |> Array.fold (fun acc elem -> 
                                        match elem.OccupiedNeighbours with
                                        | Some n when n < 4 -> acc + 1
                                        | _ -> acc
                                    ) 0
                    )

                Expect.equal totalAccessible 13 "Total accessible elements should be 13"
        ] 

        testList "Part 2 tests" [
            testCase "All test data 2 loops should get 12 result" <| fun _ ->    
                let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 5}

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                    |> buildPaperRollMetadataArray
                    |> removeRolls
                    |> fun (removedCount, remaining) -> 
                        buildPaperRollMetadataArray remaining
                    
                let totalAccessible = 
                    result
                    |> Array.sumBy (fun row -> 
                        row 
                        |> Array.fold (fun acc elem -> 
                                        match elem.OccupiedNeighbours with
                                        | Some n when n < 4 -> acc + 1
                                        | _ -> acc
                                    ) 0
                    )

                Expect.equal totalAccessible 12 "Total accessible elements should be 12"

            testCase "All test data 3 loops should get 7 result" <| fun _ ->    
                let expected = {SlotStatus = Occupied; OccupiedNeighbours = Some 5}

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                    |> buildPaperRollMetadataArray
                    |> removeRolls
                    |> fun (removedCount, remaining) -> 
                        buildPaperRollMetadataArray remaining
                    |> removeRolls
                    |> fun (removedCount, remaining) -> 
                        buildPaperRollMetadataArray remaining
                    
                let totalAccessible = 
                    result
                    |> Array.sumBy (fun row -> 
                        row 
                        |> Array.fold (fun acc elem -> 
                                        match elem.OccupiedNeighbours with
                                        | Some n when n < 4 -> acc + 1
                                        | _ -> acc
                                    ) 0
                    )

                Expect.equal totalAccessible 7 "Total accessible elements should be 7"

            testCase "All test data should get 43 result" <| fun _ ->    
                let expected = 43

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map string
                    |> buildPaperRollArray
                    |> buildPaperRollMetadataArray
                    |>iterateRollRemoval
                    |> fst


                Expect.equal result expected $"Total accessible elements should be {expected}"

        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [|CLIArguments.Filter_Test_List("Part 2 tests")|]