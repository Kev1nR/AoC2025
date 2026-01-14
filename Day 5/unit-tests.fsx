#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 5\process.fsx"

open Expecto 
open Process

let inputdata = "3-5
10-14
16-20
12-18

1
5
8
11
17
32"

// Define tests
let tests =
    testList "Day 5 tests" [
        testList "Part 1 tests" [
            testCase "Can build a set of fresh ranges from inputdata" <| fun _ ->
                let expectedRanges = 
                    seq {
                        { Start = 12L; End = 18L }
                        { Start = 16L; End = 20L }
                        { Start = 10L; End = 14L }
                        { Start = 3L; End = 5L }
                    } |> Seq.toList

                let expectedIngredients = 
                    seq {
                        32L
                        17L
                        11L
                        8L
                        5L
                        1L
                    } |> Seq.toList
                    
                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> parseData
                    
                Expect.equal (fst result) expectedRanges "Ranges do not match"
                Expect.equal (snd result) expectedIngredients "Ingredients do not match"
            
            testCase "Can check freshness of ingredients" <| fun _ ->
            
                let freshRanges = 
                    seq {
                        { Start = 3L; End = 5L }
                        { Start = 10L; End = 14L }
                        { Start = 16L; End = 20L }
                    } |> Seq.toList

                let ingredients = 
                    seq {
                        1L
                        5L
                        8L
                        11L
                        17L
                        32L
                    } |> Seq.toList

                let expectedFreshIngredients = 
                    seq {
                        5L
                        11L
                        17L
                    } |> Seq.toList

                let result = (freshRanges, ingredients) ||> checkFreshness

                Expect.equal result expectedFreshIngredients "Fresh ingredients do not match" 

            ]
        
        testList "Part 2 tests" [
            testCase "Can expand ranges to count unique fresh ingredients" <| fun _ ->
                let freshRanges = 
                    seq {
                        { Start = 3L; End = 5L }
                        { Start = 10L; End = 14L }
                        { Start = 16L; End = 20L }
                        { Start = 12L; End = 18L }
                    } |> Seq.toList

                let expectedCount = 14 

                let result = 
                    squash freshRanges
                    |> List.sumBy (fun range -> range.End - range.Start + 1L)

                Expect.equal result expectedCount "Count of unique fresh ingredients does not match"
        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]