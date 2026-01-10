#r "nuget: Expecto, 9.0.4" // Load Expecto from NuGet
#load @".\AoCUtils.fsx"

open Expecto
open AoCUtils.Utils

let inputdata = 
    [|
        "..@@.@@@@."
        "@@@.@.@.@@"
        "@@@@@.@.@@"
        "@.@@@@..@."
        "@@.@@@@.@@"
        ".@@@@@@@.@"
        ".@.@.@.@@@"
        "@.@@@.@@@@"
        ".@@@@@@@@."
        "@.@.@@@.@."
    |]

let inputdata2 = 
    [|
        "..@@"
        "@@@."
        "@@@@"
        "@.@@"
    |]

let inputdata3 = 
    [|
        ".."
        "@@"
    |]

// Example function to test

type Slot = Occupied | Empty

// Define tests
let tests =
    testList "Initialisation tests" [
        testCase "Build initialised array from text input" <| fun _ ->
            let initFunc c = 
                if c = '@' 
                then
                    (c, Some c)
                else
                    (c, None)

            let results =
                inputdata
                |> Array.map (fun line -> line.ToCharArray())
                |> buildMatrixMetadata initFunc

            Expect.equal results.Length 10 "Result array should have 10 rows"
            Expect.equal (results[0][0]) ('.', None) "Empty element expected"
            Expect.equal (results[0][2]) ('@', Some '@') "Occupied element expected"

        testCase "Update matrix metadata" <| fun _ ->
            let initFunc c = 
                if c = '@' 
                then
                    (c, Occupied, "from init")
                else
                    (c, Empty, "from init")

            let updateFunc row col metadata = 
                let (c, status, _) = metadata
                
                match status with
                | Occupied -> (c, Occupied, $"from update of {row},{col}")
                | Empty -> (c, Empty, $"from update of {row},{col}") 
                
            let results =
                inputdata
                |> Array.map (fun line -> line.ToCharArray())
                |> buildMatrixMetadata initFunc
                |> updateMatrixMetadata updateFunc

            Expect.equal results.Length 10 "Result array should have 10 rows"
            Expect.equal (results[0][0]) ('.', Empty, "from update of 0,0") "Empty element expected"
            Expect.equal (results[0][2]) ('@', Occupied, "from update of 0,2") "Occupied element expected"

        testCase "Update matrix metadata across rows and columns" <| fun _ ->
            let initFunc c = 
                if c = '@' 
                then
                    (c, Occupied, "from init")
                else
                    (c, Empty, "from init")

            let updateFunc (r : int) (c' : int) (neighbours : (char * Slot * string) array array) metadata = 
                let (_, status, _) = metadata
                
                match status with
                | Occupied -> 
                    let mutable count = 0
                    for r in 0 .. neighbours.Length - 1 do
                        for c in 0 .. neighbours[0].Length - 1 do
                            let (_, neighbour_slot, _) = neighbours[r][c]
                            match neighbour_slot with
                            | Occupied -> 
                                count <- count + 1
                            | Empty -> 
                                count <- count

                    ('@', Occupied, $"from update of {r},{c'} found {count} occupied neighbours")
                    
                | Empty -> ('.', Empty, $"from update of {r},{c'}") 

            let results =
                inputdata
                |> Array.map (fun line -> line.ToCharArray())
                |> buildMatrixMetadata initFunc
                |> updateMatrixMetadataFromNeighbours updateFunc

            Expect.equal results.Length 10 "Result array should have 10 rows"
            Expect.equal (results[0][0]) ('.', Empty, "from update of 0,0") "Empty element expected"
            Expect.equal (results[0][2]) ('@', Occupied, "from update of 0,2 found 4 occupied neighbours") "Occupied element expected"

    ] 

// Run tests
// [<EntryPoint>]
let main argv =
    runTestsWithArgs defaultConfig argv tests

main [||] 