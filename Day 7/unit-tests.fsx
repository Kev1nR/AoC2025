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
                    "Expected failure due to missing beam initiator"

            testCase "Finds beam origin" <| fun _ ->
                let expectedBeams = [{PathCount = 1; Path = 7}]

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

                let currentBeams = [5; 7] |> List.map (fun p -> {PathCount = 1; Path = p })
                let actualBeams = 
                    locateSplitters  currentBeams input[1]

                Expect.equal actualBeams expectedBeams "Actual beams does not match expected"

            testCase "Row with a hit splitter returns hit locations" <| fun _ ->
                let expectedBeams = [{PathCount = 0; Path = 7}]

                let input = 
                    testData1.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())

                let actualBeams = 
                    locateSplitters  
                        ([5; 7] |> List.map (fun p -> {PathCount = 0; Path = p}))
                        input[2]

                Expect.equal actualBeams expectedBeams "Actual beams does not match expected"

            testCase "Given beam list and single new hit, a new beamData item is generated" <| fun _ ->
                let currentBeamData = 
                    {Hits = 1; 
                     Beams = [4;6;9] |> List.map (fun p -> {PathCount = 0; Path = p})}

                let expectBeamData = 
                    {Hits = 2; 
                     Beams = [4;5;7;9]|> List.map (fun p -> {PathCount = 0; Path = p})}

                let newBeamData = updateBeamData currentBeamData [6]
                
                Expect.equal newBeamData expectBeamData "New beam data does not match epxectation"

            testCase "Given beam list and two new hits, a new beamData item is generated" <| fun _ ->
                let currentBeamData = 
                    {Hits = 1; 
                     Beams = [4;6;9] |> List.map (fun p -> {PathCount = 0; Path = p})}

                let expectBeamData = 
                    {Hits = 3; 
                     Beams = [4;5;7;8;10] |> List.map (fun p -> {PathCount = 0; Path = p})}

                let newBeamData = updateBeamData currentBeamData [6; 9]
                
                Expect.equal newBeamData expectBeamData "New beam data does not match epxectation"

            testCase "Given beam list and single new hit that causes duplication, only distinct beams are generated" <| fun _ ->
                let currentBeamData = 
                    {Hits = 1; 
                     Beams = [4;6;7] |> List.map (fun p -> {PathCount = 0; Path = p})}

                let expectBeamData = 
                    {Hits = 2; Beams = [4;5;7] |> List.map (fun p -> {PathCount = 0; Path = p})}

                let newBeamData = updateBeamData currentBeamData [6]
                
                Expect.equal newBeamData expectBeamData "New beam data does not match epxectation"

            testCase "First row Beam initiation returns a Beam" <| fun _ ->
                let expectBeamData = 
                    {Hits = 0; 
                     Beams = [7] |> List.map (fun p -> {PathCount = 1; Path = p})}

                let p1Result = 
                    sampleData.Split(Environment.NewLine)
                    |> Array.take 1
                    |> Array.map(fun line -> line.ToCharArray())
                    |> processData

                Expect.equal p1Result expectBeamData "Part 1 first line result does not match expectation"

            testCase "Run against complete test date produces expected result" <| fun _ ->
                let expectHits = 21

                let p1Result = 
                    sampleData.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())
                    |> processData

                Expect.equal p1Result.Hits expectHits "Part 1 result does not match expectation"
        ]
        
        testList "Part 2 tests" [
            testCase "First splitter is path count 1 " <| fun _ ->
                let currentBeamData = 
                    {Hits = 0; 
                     Beams = [7] |> List.map (fun p -> {PathCount = 1; Path = p})}

                let expected= 
                    {Hits = 1; 
                     Beams = [{PathCount = 1; Path = 6}; {PathCount = 1; Path = 8}]}

                let newBeamData = updateBeamData currentBeamData [7]

                Expect.equal newBeamData expected (sprintf "Expectation not matched. Expected %An got %A" expected newBeamData)

            testCase "Multi parents sum up" <| fun _ ->
                let currentBeamData = 
                    {Hits = 0; 
                     Beams = [7; 9] |> List.map (fun p -> {PathCount = 1; Path = p})}

                let expected= 
                    {Hits = 2; 
                     Beams = [{PathCount = 1; Path = 6}; {PathCount = 2; Path = 8}; {PathCount = 1; Path = 10}]}

                let newBeamData = updateBeamData currentBeamData [7;9]

                Expect.equal newBeamData expected (sprintf "Expectation not matched. Expected %An got %A" expected newBeamData)

            testCase "Run against complete test date produces expected result" <| fun _ ->
                let expectPaths = 40

                let p1Result = 
                    sampleData.Split(Environment.NewLine)
                    |> Array.map(fun line -> line.ToCharArray())
                    |> processData
                    |> fun bdat -> bdat.Beams |> List.sumBy (fun b -> b.PathCount)


                Expect.equal p1Result expectPaths "Part 2 result does not match expectation"
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]