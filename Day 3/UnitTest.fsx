// #load @"C:\Users\kevin\source\AoC2025\Day 3\process.fsx"
#load @"..\Day 3\process.fsx"
#r @"nuget: Xunit" 
#r @"nuget: FsUnit.Xunit"

module Tests =
    open Xunit
    open FsUnit.Xunit
    open Process
 
    [<Theory>]
    [<InlineData("987654321111111", 98L)>]
    [<InlineData("811111111111119", 89L)>]
    [<InlineData("234234234234278", 78L)>]
    [<InlineData("818181911112111", 92L)>]
    let ``Part 1 Sample data line should return expected`` (data : string, expected : int64) =
        let result = parseLine_part1 data  
        result |> should equal expected 

    [<Theory>]
    [<InlineData("987654321111111", 987654321111L)>]
    [<InlineData("811111111111119", 811111111119L)>]
    [<InlineData("234234234234278", 434234234278L)>]
    [<InlineData("818181911112111", 888911112111L)>]
    let ``Part 2 Sample data line should return expected`` (data : string, expected : int64) =
        let result = parseLine_part2 data  
        result |> should equal expected 

    
[
    ("987654321111111", 98L)
    ("811111111111119", 89L)
    ("234234234234278", 78L)
    ("818181911112111", 92L)
]
|> List.iter (fun (data, expected) ->
    Tests.``Part 1 Sample data line should return expected``(data, expected))

[
    ("987654321111111", 987654321111L)
    ("811111111111119", 811111111119L)
    ("234234234234278", 434234234278L)
    ("818181911112111", 888911112111L)
]
|> List.iter (fun (data, expected) ->
    Tests.``Part 2 Sample data line should return expected``(data, expected))
