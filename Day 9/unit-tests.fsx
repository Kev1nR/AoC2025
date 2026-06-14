#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 9\process.fsx"

open System
open Expecto 
open AoCUtils.Utils
open Process

let sampleData = 
    "7,1
11,1
2,3
7,3
2,5
9,5
9,7
11,7
"

// Define tests
let tests =
    testList "Day 9 tests" [
        testList "Part 1 tests" [
            testCase "Can build a jagged array sequence of areas" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> 
                        s.Split(','))
                    |> Seq.map (fun elem  -> 
                        match elem with
                        | [|a; b|] -> (int64 a, int64 b)
                        | _ -> failwith "Unexpected corrdinates") 
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
                    |> Seq.map (fun elem  -> 
                        match elem with
                        | [|a; b|] -> (int64 a, int64 b)
                        | _ -> failwith "Unexpected corrdinates") 
                    |> Seq.toArray

                let res = 
                    pairwiseFold calcAreas inputData
                    |> Seq.maxBy (fun (_,_, A) -> A)
                    |> fun (_, _, A) -> A
               
                Expect.equal res 50 "Incorrect max area"
                
        ]
        
        testList "Part 2 tests" [
            let inpolygon_tests = 
                [
                    ((7L, 1L), (2L, 3L), false)
                    ((11L, 1L), (9L, 7L), true)              
                    ((11L, 1L), (11L, 7L), false)              
                    ((2L, 3L), (7L, 3L), false)                
                    ((2L, 3L), (2L, 5L), false)                
                    ((2L, 3L), (9L, 5L), true)               
                    ((2L, 3L), (9L, 7L), false)               
                    ((2L, 3L), (11L, 7L), false)              
                    ((7L, 3L), (2L, 5L), true)               
                    ((7L, 3L), (9L, 5L), true)                
                    ((7L, 3L), (9L, 7L), false)               
                    ((7L, 3L), (11L, 7L), false)              
                    ((2L, 5L), (9L, 5L), false)                
                    ((2L, 5L), (9L, 7L), false)               
                    ((2L, 5L), (11L, 7L), false)              
                    ((9L, 5L), (9L, 7L), false)                
                    ((9L, 5L), (11L, 7L), true)               
                    ((9L, 7L), (11L, 7L), false)
                ]
            
            testList "Rectangle in boundary tests" [
                for (rect_c1, rect_c2, expected) in inpolygon_tests do
                    testCase $"""Rectangle {rect_c1}, {rect_c2} should {if expected then "be" else "not be"} in boundary""" <| fun _ ->
                        let inputData = 
                            sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                            |> Seq.map (fun s -> 
                                s.Split(','))
                            |> Seq.map (fun elem  -> 
                                match elem with
                                | [|a; b|] -> (int64 a, int64 b)
                                | _ -> failwith "Unexpected corrdinates") 
                            |> Seq.toArray

                        let verticalEdges = 
                            getVerticalEdges inputData
                            |> Array.sortBy (fun e -> e.Base, e.Max, e.Min)

                        let horizontalEdges = 
                            getHorizontalEdges inputData
                            |> Array.sortBy (fun e -> e.Base, e.Max, e.Min)

                        let actual = rectInPolygon verticalEdges horizontalEdges (rect_c1, rect_c2)   

                        Expect.equal actual expected $"""Point {if expected then "is" else "is not"} expected to be in polygon"""

            ]

            testCase "Detect a horizontal crossing for (7,3), (9,7)" <| fun _ ->
                let horizontalEdges = 
                    [|
                        {
                            Orientation = Horizontal
                            Base = 5
                            Max = 9
                            Min = 2
                        }
                    |]
                    
                let rect  = getRectCorners ((7,3), (9,7))
                let actual = detectHorizontalCrossing horizontalEdges rect

                Expect.isTrue actual "Shoud have detected a horizontal crossing"

            testCase "Detect a horizontal crossing for (2,3), (9,7)" <| fun _ ->
                let horizontalEdges = 
                    [|
                        {
                            Orientation = Horizontal
                            Base = 5
                            Max = 9
                            Min = 2
                        }
                    |]
                    
                let rect  = getRectCorners ((2,3), (9,7))
                let actual = detectHorizontalCrossing horizontalEdges rect

                Expect.isTrue actual "Shoud have detected a horizontal crossing"

            testCase "Generate a list of vertical edges for the bounding polygon" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> 
                        s.Split(','))
                    |> Seq.map (fun elem  -> 
                        match elem with
                        | [|a; b|] -> (int64 a, int64 b)
                        | _ -> failwith "Unexpected coordinates") 
                    |> Seq.toArray

                let expectedEdges = 
                    [|
                        { Orientation = Orientation.Vertical; Base = 7L; Max = 3L; Min = 1L }
                        { Orientation = Orientation.Vertical; Base = 2L; Max = 5L; Min = 3L }
                        { Orientation = Orientation.Vertical; Base = 11L; Max = 7L; Min = 1L }
                        { Orientation = Orientation.Vertical; Base = 9L; Max = 7L; Min = 5L }
                    |]
                    |> Array.sortBy (fun e -> e.Base, e.Max, e.Min)

                let verticalEdges = 
                    getVerticalEdges inputData
                    |> Array.sortBy (fun e -> e.Base, e.Max, e.Min)

                Expect.sequenceEqual verticalEdges expectedEdges "Expected vertical edges to be the same"

            testCase "Generate a list of horizontal edges for the bounding polygon" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> 
                        s.Split(','))
                    |> Seq.map (fun elem  -> 
                        match elem with
                        | [|a; b|] -> (int64 a, int64 b)
                        | _ -> failwith "Unexpected corrdinates") 
                    |> Seq.toArray

                let expectedEdges = 
                    [|
                        { Orientation = Orientation.Horizontal; Base = 1L; Max = 11L; Min = 7L }
                        { Orientation = Orientation.Horizontal; Base = 3L; Max = 7L; Min = 2L }
                        { Orientation = Orientation.Horizontal; Base = 5L; Max = 9L; Min = 2L }
                        { Orientation = Orientation.Horizontal; Base = 7L; Max = 11L; Min = 9L }
                    |]
                    |> Array.sortBy (fun e -> e.Base, e.Max, e.Min)

                let horizontalEdges = 
                    getHorizontalEdges inputData
                    |> Array.sortBy (fun e -> e.Base, e.Max, e.Min)

                Expect.sequenceEqual horizontalEdges expectedEdges "Expected horizontal edges to be the same"
            
            testCase "Part 2 gives correct result" <| fun _ ->
                let inputData = 
                    sampleData.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> 
                        s.Split(','))
                    |> Seq.map (fun elem  -> 
                        match elem with
                        | [|a; b|] -> (int64 a, int64 b)
                        | _ -> failwith "Unexpected corrdinates") 
                    |> Seq.toArray

                let expected = 24L

                let _, _, actual = part2 inputData

                Expect.equal actual expected "Incorrect result"
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]