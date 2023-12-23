#load "Common.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

type NumberPosition = { StartX: int; EndX: int; Y: int }
type SymbolPosition = { X: int; Y: int }
type Number = { Value: int; Position: NumberPosition }
type Symbol = { Value: char; Position: SymbolPosition }

let (|Number|_|) (str:string) =
    match Int32.TryParse str with
    | (true, v) -> Some v
    | _ -> None
    
let (|Symbol|_|) (s:string) =
    match s.Length with
    | 1 -> Some s[0]
    | _ -> None
    
let partition =
    let rec loop (acc1, acc2) = function
        | [] -> (List.rev acc1, List.rev acc2)
        | x :: xs ->
            match x with
            | Choice1Of2 n -> loop (n :: acc1, acc2) xs
            | Choice2Of2 s -> loop (acc1, s :: acc2) xs
    
    loop ([], []) 
    
let parse (lines:string[]) =
    lines
    |> Array.indexed
    |> Array.collect (fun (idx, line) ->
        Regex.Matches(line, "\d+|[^.]")
        |> Seq.map (fun m ->
            {|Y=idx;X=m.Index;Value=m.Value|})
        |> Seq.toArray)
    |> Array.map (fun v ->
        match v.Value with
        | Number n -> Choice1Of2 { Number.Value=n; Position={StartX=v.X;EndX=v.X+v.Value.Length-1;Y=v.Y } }
        | Symbol s -> Choice2Of2 { Value=s; Position = {X=v.X; Y=v.Y}}
        | _ -> failwith "invalid value")
    |> Array.toList
    |> partition
    
let isAdjacentTo (n:NumberPosition) (s: SymbolPosition) =
    let left = abs (n.StartX - s.X) <= 1
    let right = abs (n.EndX - s.X) <= 1
    let y = abs (s.Y - n.Y) <= 1
    (left || right) && y
    
let calculateTotal (numbers: Number list, symbols) =
    numbers
    |> List.choose (fun n ->
        let isPartNumber =
            symbols |> List.exists (fun s -> (isAdjacentTo n.Position s.Position))
    
        if isPartNumber then Some n.Value else None)
    |> List.sum

let sample =
 """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""

let file = "AdventOfCode2023/data/day3.txt"
let parsedSample = sample.SplitOnNewLine() |> parse
let parsedFile = File.ReadAllLines file |> parse

parsedSample |> calculateTotal
parsedFile |> calculateTotal


// Part 2

let getGearRatioTotal (numbers: Number list, symbols) =
    symbols
    |> List.filter (fun s -> s.Value = '*')// get gears
    |> List.map (fun s ->
        let adjacentParts =
            numbers
            |> List.filter (fun n -> isAdjacentTo n.Position s.Position)
    
        match adjacentParts with
        | [p1;p2] -> p1.Value*p2.Value
        | _ -> 0)
    |> List.sum
    
parsedSample |> getGearRatioTotal
parsedFile |> getGearRatioTotal