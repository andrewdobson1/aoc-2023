#load "Common.fsx"

open System.Collections.Generic

type Direction =
    | North
    | South
    | East
    | West
    
type Position = {
    X: int
    Y: int
} with
    member this.Move direction =
        match direction with
        | North -> { this with Y=this.Y-1}
        | South -> { this with Y=this.Y+1}
        | East -> { this with X=this.X+1}
        | West -> { this with X=this.X-1}
    
type Pipe =
    | NorthSouth
    | EastWest
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Ground
    | Start
    
let (|Pipe|) (c:char) =
    match c with
    | '|' -> NorthSouth
    | '-' -> EastWest
    | 'L' -> NorthEast
    | 'J' -> NorthWest
    | '7' -> SouthWest
    | 'F' -> SouthEast
    | '.' -> Ground
    | 'S' -> Start
    | _ -> failwith $"invalid input {c}"   

type Tile = {
    Pipe: Pipe
    Position: Position
}

type Field = {
    Tiles: IReadOnlyDictionary<Position, Tile>
} with
    member this.Move from direction =
        let pos = from.Position.Move direction
        match this.Tiles.ContainsKey pos with
        | true -> Some (this.Tiles[pos], direction)
        | false -> None
        
let toTiles y (line:string) =
    line.ToCharArray()
    |> Array.indexed
    |> Array.map (fun (x, c) -> 
        match c with
        | Pipe p -> { Pipe=p; Position = { X=x; Y=y } })
    
let toField (tiles: Tile array) : Field =
    tiles
    |> Array.map (fun tile -> (tile.Position, tile))
    |> (fun x -> {Tiles=readOnlyDict x})
    
let findStart (field:Field) =
    field.Tiles
    |> Seq.find (fun kv ->
        match kv.Value.Pipe with
        | Start -> true
        | _ -> false)
    |> _.Value
    
let move (field:Field) (from:Tile, direction:Direction) =
    match from.Pipe with
    | NorthSouth ->
        match direction with
        | North -> Some (field.Tiles[from.Position.Move North], North)
        | South -> Some (field.Tiles[from.Position.Move South], South)
        | _ -> None
    | EastWest ->
        match direction with
        | East -> Some (field.Tiles[from.Position.Move East], East)
        | West -> Some (field.Tiles[from.Position.Move West], West)
        | _ -> None      
    | NorthEast ->
        match direction with
        | South -> Some (field.Tiles[from.Position.Move East], East)
        | West -> Some (field.Tiles[from.Position.Move North], North)
        | _ -> None
    | NorthWest ->
        match direction with
        | South -> Some (field.Tiles[from.Position.Move West], West)
        | East -> Some (field.Tiles[from.Position.Move North], North)
        | _ -> None
    | SouthWest ->
        match direction with
        | North -> Some (field.Tiles[from.Position.Move West], West)
        | East -> Some (field.Tiles[from.Position.Move South], South)
        | _ -> None
    | SouthEast ->
        match direction with
        | North -> Some (field.Tiles[from.Position.Move East], East)
        | West -> Some (field.Tiles[from.Position.Move South], South)
        | _ -> None
    | _ -> None
    
let routes (field:Field) =
    let start = findStart field
    
    let rec loop field (tile, direction) acc =
        match move field (tile, direction)  with
        | Some (t,d) -> loop field (t, d) (t::acc)
        | None -> acc
    
    [|
        field.Move start North
        field.Move start South
        field.Move start East
        field.Move start West
    |]
    |> Array.choose id
    |> Array.map (fun next ->
        loop field next [start])
    
let sample =
    """.....
.S-7.
.|.|.
.L-J.
.....
"""

let sample2 =
    """..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"""

let parse lines =
    lines
    |> Array.indexed
    |> Array.collect (fun (y, line) -> toTiles y line)
    |> toField
    |> routes
    |> Array.map _.Length
    |> Array.max
    |> (fun x -> x/2)
    
    
sample.SplitOnNewLine() |> parse
sample2.SplitOnNewLine() |> parse
Files[10] |> parse

// Part 2

// Use shoelace formula to get the area
let area (points: Position list) =
    let rec loop acc pts =
        match pts with
        | p1 :: p2 :: rest -> loop (acc + (p1.X * p2.Y - p2.X * p1.Y)) (p2 :: rest)
        | _ -> acc

    let wrappedPoints = points @ [points.Head]  // Wrap around to the first point
    let area = loop 0 wrappedPoints
    abs(area) / 2  // Halve and take the absolute value

// Use Picks's theorem to get the interior points (A = i + b/2 - 1)
let interiorPoints boundaryPts area =
    area + 1 - boundaryPts/2
    
let sample3 =
    """.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"""

let parse' lines =
    lines
    |> Array.indexed
    |> Array.collect (fun (y, line) -> toTiles y line)
    |> toField
    |> routes
    |> Array.sortByDescending _.Length
    |> Array.head
    |> (fun tiles ->
        tiles
        |> List.map _.Position
        |> area
        |> interiorPoints (tiles.Length-1) )

sample3.SplitOnNewLine() |> parse'
Files[10] |> parse'

