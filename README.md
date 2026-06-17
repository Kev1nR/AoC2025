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
Observation: The original implementation of `pairwiseFold` accepted a sequence but the performance was very poor. I modifed to accept an Array and performance was order of magnitude faster. Obvious in hindsight as the `Seq` implementation was repeatedly scanning for the required index Cf the array direct indexing.

Part 2 : Wow! Sooooo looonnnggg. 4 months since I completed part 1 (not continuously working on it, obviously).\
This was a very difficult challenge. I initially considered the brute force approach of marking off all the cells between the corner tiles and checking whether or not all the rectangle's tiles were marked (green or red). This was fine for the sample data, but obviously not scaleable to the full data set.\
I researched some options and came across the idea of "Ray casting". I liked this idea not least becuae I'd used something similar (although I didn't know it had a name) on a previous AoC challenge.\
There are two main challenges here:

1. Cast a ray and count how many edges it crosses. If the count is odd then you're within the boundary, even, outside.
2. This polygon might be concave i.e. it might have inlets that make some section of the rectangle be outside the boundary even if the ray cast indicates it is inside. These inlets might be vertical or horizontal so we need to detect an edge crossing the edge of our test rectangle in either orientation.

**Solution to 1.** Find the rectangle's Top Left corner move a little bit inside the rectangle (i.e. I set this to TopLeft X + 0.5, TopLeft Y + 0.5) and cast from there. Count edge crossings. \
**Note** for this you need to supply a list of edges alomg with your test rectangle.

**Solution to 2.** This needs a list of vertical and horizontal edges along with the test rectangle. Then check none of the edges cross any of your rectangle edges.

##### Types implemented
- **Edge** A record type defining an edge. Orientation, base position, min and max extent.
- **Rect** Defines a rectangles 4 corner coordinates

##### Functions implemented
- **calcAreas**: `: int64 * int64 -> int64 * int64 -> int64 * int64 * int64 * int64 * int64`
Takes 2 corner coords. Returns the area of the enclosed rectangle
- **getRectCorners**: `(int64 * int64) * (int64 * int64) -> Rect
Takes 2 corner coords. Returns the Rect record of 4 corner coords
- **getHorizontalEdges**: `(int64 * int64) array -> Edge array`
Takes an array of coords. Returns an array of horizontal edges.
- **getVerticalEdges**: `(int64 * int64) array -> Edge array`
Takes an array of coords. Returns an array of vertical edges.
- **pointInPolygon**: `edges: seq<Edge> -> px : Int64 * py : Int64 -> bool`
Given a sequence of vertical edges and x and y point coords. Casts a ray to the right and counts edge crossings. Returns `true` when the number of crossings is odd, `false` otherwise.
- **detectVerticalCrossing**: `v_edges : seq<Edge> -> rect : Rect -> bool`
Given a list of vertical edges and a rectangle, determines if the rectangle edges are crossed by any of the edges.
- **detectHorizontalCrossing**: `h_edges : seq<Edge> -> rect : Rect -> bool`
Given a list of horizontal edges and a rectangle, determines if the rectangle edges are crossed by any of the edges.
**Note**: To ensure edge points on the rectangle boundary are counted correctly, the check is done against a slightly internal rectangle (0.5) smaller at each edge. 
- **rectInPolygon**: `v_edges: seq<Edge> -> h_edges: seq<Edge> -> rect_c1: int64 * int64 * rect_c2: int64 * int64 -> bool`
Returns the result of `isInPolygon`, `detectVerticalCrossing` and `detectHorizontalCrossing`.

#### Execution time (includes data read)
Part 1 result: ((83937L, 84866L), (12865L, 18149L), **4741848414L**)
Real: 00:00:00.031, CPU: 00:00:00.031, GC gen0: 1, gen1: 0, gen2: 0

Part 2 result: ((6073L, 67455L), (94582L, 50408L), 1508918480L)
Real: 00:00:00.534, CPU: 00:00:00.687, GC gen0: 2, gen1: 1, gen2: 0
