#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 8\process.fsx"

open System
open System.Numerics
open Expecto 
open Process

let sampleData = 
    "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"

// Define tests
let tests =
    testList "Day 8 tests" [
        testList "Part 1 tests" [
            testCase "Can build a sorted list of distances" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.take 3
                    |> Array.map (fun triplet ->
                        triplet.Split(','))
                
                let expected = 
                    [|
                        { FromIndex = 0; ToIndex = 1; Distance = 787.8140869f }
                        { FromIndex = 0; ToIndex = 2; Distance = 908.7843628f }
                        { FromIndex = 1; ToIndex = 2; Distance = 1019.987244f }
                    |]
               
                let actual = inputData |> sortedDistances |> Seq.toArray

                Expect.equal actual expected "Result does not match expectations"

            testCase "Can build a sorted list of distances - whole list" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun triplet ->
                        triplet.Split(','))

                let expectedItems = ((20 * 20) - 20) / 2

                let actual = inputData |> sortedDistances |> Seq.toArray

                Expect.equal (actual |> Array.length) expectedItems "Result does not match expectations"
                Expect.isAscending (actual |> Array.map (fun i -> i.Distance)) "List is noto sorted by distance" 

            testCase "Can build a list of connections" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun triplet ->
                        triplet.Split(','))

                let expected = 40

                let actual = 
                    inputData 
                    |> sortedDistances 
                    |> Array.take 10
                    |> proc1

                Expect.equal actual expected (sprintf "Expected %d but got %d" expected actual)

        ]
        
        testList "Part 2 tests" [
            testCase "Detect first point at which all junction boxes are connected into one circuit" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map (fun triplet ->
                        triplet.Split(','))

                let expected = 25272

                let distances =
                    inputData 
                    |> sortedDistances 
                    
                let actual = 
                    distances
                    |> proc2 20
                    
                Expect.equal actual actual (sprintf "Expected %A but got %A" expected actual)
                
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]