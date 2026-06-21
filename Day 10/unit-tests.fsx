#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 10\process.fsx"

open System
open Expecto 
open AoCUtils.Utils
open Process

let sampleData = 
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"

// Define tests
let tests =
    testList "Day 10 tests" [
        testList "Part 1 tests" [
            testCase "Convert input string light states to binary uints" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun line -> line |> generateLightsandButtons)

                let expectedLightStates = 
                    [uint 6u; 2u; 29u] |> List.toSeq
                    
                let actualLightStates = 
                    inputData 
                    |> Seq.map (fun ls -> ls.ExpectedLightState)

                actualLightStates 
                |> Seq.zip expectedLightStates
                |> Seq.iter (fun (act, exp) ->
                    Expect.equal act exp "Expected act = exp")

            testCase "Convert input string list of button to array of binary uints" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun line -> line |> generateLightsandButtons)
                
                let expectedButtonStates = 
                    [
                        [|1u; 5u; 2u; 3u; 10u; 12u|]
                        [|23u; 6u; 17u; 28u; 15u|]
                        [|62u; 38u; 59u; 24u|]
                    ] |> List.toSeq

                let actualButtonStates = 
                    inputData
                    |> Seq.map (fun lbs -> lbs.LightButtons)
                
                actualButtonStates 
                |> Seq.zip expectedButtonStates
                |> Seq.iter (fun (act, exp) ->
                    Expect.equal act exp "Expected act = exp")

            testList "" [
                let lightsButtons = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun line -> line |> generateLightsandButtons)
                    |> Seq.mapi (fun i lb -> (i, lb))
                    |> Seq.zip [2; 3; 2]
                    |> Seq.map (fun (exp, (tid, sts)) -> tid, sts, exp)

                for (testId, states, expected) in lightsButtons do
                    testCase $"""Identify the smallest number of button presses - {testId}""" <| fun _ ->
                        let actual = getLeastButtonPresses states.ExpectedLightState states.LightButtons

                        Expect.equal actual expected $"""Least buttons should be {expected}"""
            ]
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]