#load "Common.fsx"

open System.Collections.Generic
    
type Point = {
    x:int64
    y:int64
}

let expandColumns n (points:Point array) =
    let xs = set [ 0L .. (points |> Array.maxBy _.x |> _.x) ]
    let xs' = points |> Array.map _.x |> Set.ofArray
    let emptyColumns = (xs - xs') |> Array.ofSeq |> Array.sortDescending
    (points, emptyColumns)
    ||> Array.fold (fun acc emptyCol ->
        acc
        |> Array.map (fun point ->
            if point.x > emptyCol then { point with x=point.x+n } else point)
        )

let expandRows n (points:Point array) =
    let ys = set [ 0L .. (points |> Array.maxBy _.y |> _.y) ]
    let ys' = points |> Array.map _.y |> Set.ofArray
    let emptyRows = (ys - ys') |> Array.ofSeq |> Array.sortDescending
    (points, emptyRows)
    ||> Array.fold (fun acc emptyRow ->
        acc
        |> Array.map (fun point ->
            if point.y > emptyRow then { point with y=point.y+n } else point)
        )
    
let expandGalaxy n points =
    points
    |> expandColumns n
    |> expandRows n
    
let toGalaxyPairs points =
    [ for i in 0 .. Array.length points - 2 do
          for j in i+1 .. Array.length points - 1 do
              yield (points[i], points[j])]
    
let shortestDistance (pointA, pointB) =
    abs(pointB.x - pointA.x) + abs(pointB.y - pointA.y)
    
let parseLine (y:int, line:string) =
    line.ToCharArray()
    |> Array.indexed
    |> Array.map (fun (i,c) ->
        match c with
        | '#' -> Some {x=int64 i;y=int64 y}
        | _ -> None)
    |> Array.choose id
    
let sample =
    """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"""

let parse lines =
    lines
    |> Array.indexed
    |> Array.collect parseLine
    |> expandGalaxy 1L
    |> toGalaxyPairs
    |> List.map shortestDistance
    |> List.sum
    
sample.SplitOnNewLine() |> parse
Files[11] |> parse

// Part 2

let parse' lines =
    lines
    |> Array.indexed
    |> Array.collect parseLine
    |> expandGalaxy 999999
    |> toGalaxyPairs
    |> List.map shortestDistance
    |> List.sum

sample.SplitOnNewLine() |> parse'
Files[11] |> parse'

