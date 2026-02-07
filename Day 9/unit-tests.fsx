#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 9\process.fsx"

open System
open Expecto 
open AoCUtils.Utils
open Process

let sampleData = 
    "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"

// Define tests
let tests =
    testList "Day 9 tests" [
        testList "Part 1 tests" [
            testCase "Can build a jagged array sequence of areas" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> 
                        s.Split(','))
                    |> Seq.map (fun [|a; b|]  -> (int64 a, int64 b)) 
                    |> Seq.toArray

                let fn a b = 
                    let a1, a2 = a
                    let b1, b2 = b
                    
                    (a, b, (Int64.Abs(b1 - a1) + 1L) * (Int64.Abs(b2 - a2) + 1L))

                let res = 
                    pairwiseFold fn inputData

                let expectedType = ((0L, 0L), (0L, 0L), 0L).GetType()

                Expect.equal (res |> Seq.length) 28 "Incorrect row count in results"
                Expect.equal  ((res |> Seq.head).GetType()) expectedType "Type mismatch in first element"  

            testCase "Can generate expected result from sample data" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> 
                        s.Split(','))
                    |> Seq.map (fun [|a; b|]  -> (int64 a, int64 b)) 
                    |> Seq.toArray

                let res = 
                    pairwiseFold calcAreas inputData
                    |> Seq.maxBy (fun (_,_, A) -> A)
                    |> fun (_, _, A) -> A
               
                Expect.equal res 50 "Incorrect max area"
                
        ]
        
        testList "Part 2 tests" [
                
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]