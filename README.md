# AoC2025
Advent of code 2025 challenges  

## Day 1
#### Synopsis

#### Rating

#### Time to complete

#### Execution time (excludes data read)


## Day 2
#### Synopsis

#### Rating

#### Time to complete

#### Execution time (excludes data read)

## Day 6
#### Synopsis

#### Rating
⭐⭐⭐⭐⭐

#### Time to complete
Part 1 : An hour or two
Part 2 : Several days. It was quite tough massaging the data with several misunderstandings which the sample data did not capture. Once I got a good solution for part2 I adapted part1 to use similar functions and refactored.

#### Execution time (includes data read)
Part 1 result is s: 6757749566978
Real: 00:00:00.008, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
Part 2 result is s: 10603075273949
Real: 00:00:00.010, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

## Day 7
#### Synopsis

#### Rating
⭐⭐⭐⭐⭐

#### Time to complete
Part 1 : An hour or two. Tracking the split beam paths and matching to the '^' char in char arrays in a sinlge pass was straightforward. 
Part 2 : 12 or so hours in total. Complete refactor to add the concpt of a 'Beam' with 'PathCount' and 'Path' was needed. Once I'd finally realised that the total paths with no gaps was the sum of the last Pascal's Triangle coefficients the single pass solution becme apparent.

#### Execution time (includes data read)
The single pass of data for both parts made this efficient and fast

Part 1 result is: 1630
Real: 00:00:00.006, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
Part 2 result is: 47857642990160
Real: 00:00:00.003, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0

## Day 8
#### Synopsis

#### Rating
⭐⭐⭐⭐

#### Time to complete
Part 1 : Several hours. Started with the idea of creating a jagged array of arrays and later went with an outer/inner loop approach - simpler, probably faster.
The `buildCircuits` function seems messier than it need be - might revisit in part 2.
I created `System.Numerics.Vector3` objects and used that class's `Distance` function rather than write my own. That was because I wanted to use integer inputs but in the end the `Vector3` class takes floats anyway - will revisit in part 2 to see if performance is better with a homegrown solution.

Part 2: Updated System.Numerics.Vector3 to inline Euclidean Distance calc. Performance was around 20% slower than with System.Numerics.Vector3

#### Execution time (includes data read)
Part 1 result: 57970
Real: 00:00:00.259, CPU: 00:00:00.359, GC gen0: 1, gen1: 1, gen2: 1
Part 2 result: 8520040659
Real: 00:00:00.655, CPU: 00:00:01.328, GC gen0: 16, gen1: 3, gen2: 2


## Day 9
#### Synopsis

#### Rating
⭐⭐⭐⭐⭐

#### Time to complete
Part 1 : 2 hours. A fairly minor adaptation of the jagged array scenario of day 8. I pulled out a `pairwiseFold` function into `AoCUtils` to calculate the areas in a single pass.
Observation: The original implementation of `pairwiseFold` accepted a sequence but the perfromance was very poor. I modifed to accept an Array and performance was order of magnitude faster. Obvious in hindsight as the `Seq` implementation was repeatedly scanning for the required index Cf the array direct indexing.

#### Execution time (includes data read)
Part 1 result: ((83937L, 84866L), (12865L, 18149L), **4741848414L**)
Real: 00:00:00.031, CPU: 00:00:00.031, GC gen0: 1, gen1: 0, gen2: 0
