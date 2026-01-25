#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 7\process.fsx"

open System
open Expecto 
open Process

let sampleData = 
    ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............."

let testData1 = 
    ".......S.......
...............
.......^......."

// Define tests
let tests =
    testList "Day 7 tests" [
        testList "Part 1 tests" [
            testCase "Cannot find beam origin" <| fun _ ->
                let input = 
                    testData1.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())

                Expect.throws  
                    (fun _ -> locateSplitters [] input[1] |> ignore)
                    "Expected falure due to missing beam initiator"

            testCase "Finds beam origin" <| fun _ ->
                let expectedBeams = [7]

                let input = 
                    testData1.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())


                let actualBeams = 
                    locateSplitters [] input[0]

                Expect.equal actualBeams expectedBeams "Actual beams does not match expected"

            testCase "Row with no splitters returns empty list" <| fun _ ->
                let expectedBeams = []

                let input = 
                    testData1.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())

                let actualBeams = 
                    locateSplitters  [5; 7] input[1]

                Expect.equal actualBeams expectedBeams "Actual beams does not match expected"

            testCase "Row with a hit splitter returns hit locations" <| fun _ ->
                let expectedBeams = [7]

                let input = 
                    testData1.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())

                let actualBeams = 
                    locateSplitters  [5; 7] input[2]

                Expect.equal actualBeams expectedBeams "Actual beams does not match expected"

            testCase "Given beam list and single new hit, a new beamData item is generated" <| fun _ ->
                let currentBeamData = {Hits = 1; Beams = [4;6;9]}

                let expectBeamData = {Hits = 2; Beams = [4;5;7;9]}

                let newBeamData = updateBeamData currentBeamData [6]
                
                Expect.equal newBeamData expectBeamData "New beam data does not match epxectation"

            testCase "Given beam list and two new hits, a new beamData item is generated" <| fun _ ->
                let currentBeamData = {Hits = 1; Beams = [4;6;9]}

                let expectBeamData = {Hits = 3; Beams = [4;5;7;8;10]}

                let newBeamData = updateBeamData currentBeamData [6; 9]
                
                Expect.equal newBeamData expectBeamData "New beam data does not match epxectation"

            testCase "Given beam list and single new hit that causes duplication, only distinct beams are generated" <| fun _ ->
                let currentBeamData = {Hits = 1; Beams = [4;6;7]}

                let expectBeamData = {Hits = 2; Beams = [4;5;7]}

                let newBeamData = updateBeamData currentBeamData [6]
                
                Expect.equal newBeamData expectBeamData "New beam data does not match epxectation"

            
        ]

        
        testList "Part 2 tests" [
            
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]