#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @"..\Day 6\process.fsx"

open Expecto 
open Process

let inputdata = 
    "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  "

// Define tests
let tests =
    testList "Day 6 tests" [
        testList "Part 1 tests" [
            testCase "Can split operators from values" <| fun _ ->
                let expectedOperators = [| "*"; "+"; "*"; "+" |]
                let expectedNumbers = 
                    [| 
                        "123 328  51 64 "
                        " 45 64  387 23 "
                        "  6 98  215 314"
                    |]

                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries) 
                    |> splitOperatorsFromValues  
                
                Expect.equal (fst result) expectedOperators  "Operators do not match"
                Expect.equal (snd result) expectedNumbers  "Numbers do not match"

            testCase "Can process full input data for part 1" <| fun _ ->
                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries) 
                    |> processP1
                    
                let expected = 
                    [|
                        123L * 45L * 6L
                        328L + 64L + 98L
                        51L * 387L * 215L
                        64L + 23L + 314L    
                    |]
                    |> Array.sum

                Expect.equal result expected "Part 1 result does not match"

            testCase "Can calculate the expected Part 1 value" <| fun _ ->
                let expected = 4277556L
                
                let result =  
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries)
                    |> processP1
                
                Expect.equal result expected "Actaul value does not match expected"
    
        ]
        
        testList "Part 2 tests" [
            testCase "Can process full input data for part 2" <| fun _ ->
                let result = 
                    inputdata.Split(System.Environment.NewLine, System.StringSplitOptions.RemoveEmptyEntries) 
                    |> processP2
                
                let expected = 3263827L

                Expect.equal result result "Part 1 result does not match"

        ]
    ]

let main argv =
    runTestsWithCLIArgs argv [||] tests

main [||]