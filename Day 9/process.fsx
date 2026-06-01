// Day 3 challenge 
#load @"..\AoCUtils.fsx"

let filePath = @"..\Day 9\input-data.txt"

open AoCUtils.Utils
open System

type Orientation = Horizontal | Vertical

type Edge = {Orientation: Orientation; Base: int64; Max: int64; Min: int64}

let inputData = 
    filePath
    |> ReadData.readLines
    |> Seq.map (fun s -> 
        s.Split(','))
    |> Seq.map (fun [|a; b|]  -> (int64 a, int64 b)) 
    |> Seq.toArray
    
let calcAreas a b = 
    let a1, a2 = a
    let b1, b2 = b
    
    (a, b, (Int64.Abs(b1 - a1) + 1L) * (Int64.Abs(b2 - a2) + 1L))

let part1 () = 
    inputData 
    |> pairwiseFold calcAreas
    |> Seq.maxBy (fun (_,_, A) -> A)
    |> printfn "%A"

let getRectCorners (rectCoords : (int64 * int64) * (int64 * int64)) =
    let r1, c1 = fst rectCoords
    let r2, c2 = snd rectCoords
    let topLeft = Math.Min (r1, r2), Math.Min(c1, c2)
    let topRight = Math.Max (r1, r2), Math.Min(c1, c2)
    let bottomLeft = Math.Min (r1, r2), Math.Max(c1, c2)
    let bottomRight = Math.Max (r1, r2), Math.Max(c1, c2)

    topLeft, topRight, bottomLeft, bottomRight

let getHorizontalEdges coords =
    coords
    |> Array.sortBy (fun (x, y) -> y)
    |> Array.fold (fun (nextEdge, edgecoords) (x, y) -> 
        match nextEdge with
        | None -> 
            let newEdgeCoord = {Orientation = Horizontal; Base = y; Max = x; Min = x}
            Some newEdgeCoord, [|newEdgeCoord|]
        | Some e -> 
            if y = e.Base then
                let newEdgeCoord = 
                    {Orientation = Horizontal; Base = y; 
                    Max = Math.Max (x, e.Max); 
                    Min = Math.Min (x, e.Min)}
                
                Some newEdgeCoord, edgecoords[1..] |> Array.insertAt 0 newEdgeCoord
            else
                let newEdgeCoord = 
                    {Orientation = Horizontal; Base = y; Max = x; Min = x}
                
                Some newEdgeCoord, edgecoords |> Array.insertAt 0 newEdgeCoord
        ) (None, [||])
    |> snd
    
let getVerticalEdges coords =
    coords
    |> Array.sortBy (fun (x, y) -> x)
    |> Array.fold (fun (nextEdge, edgecoords) (x, y) -> 
        printfn "Next edge: %A" nextEdge
        match nextEdge with
        | None -> 
            let newEdgeCoord = {Orientation = Vertical; Base = x; Max = y; Min = y}
            Some newEdgeCoord, [|newEdgeCoord|]
        | Some e -> 
            if x = e.Base then
                let newEdgeCoord = 
                    {Orientation = Vertical; Base = x; 
                    Max = Math.Max (y, e.Max); 
                    Min = Math.Min (y, e.Min)}
                Some newEdgeCoord, edgecoords[1..] |> Array.insertAt 0 newEdgeCoord
            else
                let newEdgeCoord = 
                    {Orientation = Vertical; Base = x; Max = y; Min = y}
                Some newEdgeCoord, edgecoords |> Array.insertAt 0 newEdgeCoord
        ) (None, [||])
    |> snd

let pointInPolygon edges (px : Int64, py : Int64) =
    let (px', py') = (float px + 0.5), (float py + 0.5)

    let crossings = 
        edges
        |> Seq.filter (fun edge -> (float edge.Base) > px' && (float edge.Max) > py' && (float edge.Min) < py')

    printfn "Crossings = %A" crossings

    (crossings |> Seq.length) % 2 = 1

let rectInPolygon v_edges h_edges (rect_c1, rect_c2) = 
    let topL, _, _, _ = getRectCorners (rect_c1, rect_c2)
    
    let isInPoly =
        pointInPolygon v_edges topL
    
    let rectIsCrossedVert = false
    let rectIsCrossedHoriz  = false

    isInPoly && (not rectIsCrossedVert) && (not rectIsCrossedHoriz)


#time
part1()
#time 

#time
#time