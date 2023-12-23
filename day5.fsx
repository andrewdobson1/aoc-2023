#load "Common.fsx"

open System
open System.IO
open System.Text.RegularExpressions

type ElementMap = {
    Dest: uint64
    SourceStart: uint64
    SourceEnd: uint64
}

type Maps = {
    SeedToSoil: ElementMap list
    SoilToFertilizer: ElementMap list
    FertilizerToWater: ElementMap list
    WaterToLight: ElementMap list
    LightToTemp: ElementMap list
    TempToHumidity: ElementMap list
    HumidityToLoc: ElementMap list
}

let parseMap mapName lines =
    ({|
       CurrentMap = None
       Output = Array.empty
    |}, lines)
    ||> Array.fold(fun state line ->
        match line, state.CurrentMap with
        | Split ':' [name] , _ when name = mapName -> {|
            state with
                CurrentMap = Some name
            |}
        | Split ' ' [ UInt64 destination; UInt64 start; UInt64 length ], Some _ ->
            {|
                state with
                    Output = Array.append
                                 state.Output
                                 [|{                                   
                                       Dest=destination
                                       SourceStart = start
                                       SourceEnd = start + length                     
                                  }|]                                                        
            |}
        | "", Some _ ->
            {|
                state with
                    CurrentMap = None                                                    
            |}
        | _ -> state
        )
    |> (fun state ->
        state.Output
        |> Array.toList)
    
let parseSeeds (lines:string array) =
    lines
    |> Array.collect (fun line ->
            match line with
            | Split ':' [ "seeds"; Split ' ' seeds ] ->
                seeds |> List.map uint64 |> List.toArray
            | _ -> [||]
        )

let parseMaps lines =
    {
        SeedToSoil = parseMap "seed-to-soil map" lines
        SoilToFertilizer = parseMap "soil-to-fertilizer map" lines
        FertilizerToWater = parseMap "fertilizer-to-water map" lines
        WaterToLight = parseMap "water-to-light map" lines
        LightToTemp = parseMap "light-to-temperature map" lines
        TempToHumidity = parseMap "temperature-to-humidity map" lines
        HumidityToLoc = parseMap "humidity-to-location map" lines
    }
    
let lookup (map:ElementMap list) value =
    let entry = map |> List.tryFind (fun x -> value >= x.SourceStart && value <= x.SourceEnd)
    match entry with
    | Some v -> v.Dest + value - v.SourceStart
    | _ -> value
    
let location maps seed =
    let soil = lookup maps.SeedToSoil seed
    let fertilizer = lookup maps.SoilToFertilizer soil
    let water = lookup maps.FertilizerToWater fertilizer
    let light = lookup maps.WaterToLight water
    let temp = lookup maps.LightToTemp light
    let humidity = lookup maps.TempToHumidity temp
    lookup maps.HumidityToLoc humidity

let sample =
    """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"""

let file = "data/day5.txt"
    
let parse lines =
    let maps = lines |> parseMaps
    
    lines
    |> parseSeeds
    |> Array.map (location maps)
    |> Array.min

//sample.SplitOnNewLine() |> parse
//sample.SplitOnNewLine() |> parseMap "soil-to-fertilizer map"
Files[5] |> parse 

// Part 2

let parseSeeds' lines =
    parseSeeds lines
    |> Array.chunkBySize 2
    |> Array.map (fun arr -> (arr[0], arr[1]))
    
let parse' lines =
    let maps = lines |> parseMaps
    
    let rec findSmallest smallest current max =
        if current > max then
            smallest
        else
            findSmallest (min smallest (location maps current)) (current + 1UL) max
    
    lines
    |> parseSeeds'
    |> Array.Parallel.map (fun (start, range) -> findSmallest UInt64.MaxValue start (start+range))
    |> Array.min
    //|> Array.collect (fun x -> Array.init (snd x) (fun i -> i))
    
sample.SplitOnNewLine() |> parse'
Files[5] |> parse'