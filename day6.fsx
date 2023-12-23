#load "Common.fsx"

[<Measure>] type ms
[<Measure>] type mm

type Race = {
    Duration: uint64<ms>
    Record: uint64<mm>
}

let sample =
    """Time:      7  15   30
Distance:  9  40  200
"""

let parseRaces lines =
    (([],[]), lines)
    ||> Array.fold (fun state line ->
        match line with
        | Split ':' ["Time"; Split ' ' times] ->
            (times |> List.map uint64, [])
        | Split ':' ["Distance"; Split ' ' distances] ->
            (fst state, distances |> List.map uint64)
        | _ -> state
        )
    |> (fun (t, d) -> List.zip t d)
    |> List.map (fun (t,d) -> {Duration=t*1UL<ms>; Record=d*1UL<mm> })

let calculateWaysToBeatRaceRecord (race:Race) =
    [ 1UL .. (uint64 race.Duration - 1UL)]
    |> List.map (fun buttonTime ->
        let speed = buttonTime * 1UL<mm/ms>
        let buttonTime = buttonTime * 1UL<ms>
        let distance = speed * (race.Duration - buttonTime)
        if distance > race.Record then 1 else 0)
    |> List.sum
        
let parse lines =
    lines
    |> parseRaces
    |> List.map calculateWaysToBeatRaceRecord
    |> List.reduce (*)

sample.SplitOnNewLine() |> parse
Files[6] |> parse

// Part 2
    
let combineRaces (races:Race list) =
    races
    |> List.map (fun r -> (r.Duration.ToString(), r.Record.ToString()))
    |> List.reduce (fun a b -> (fst a + fst b, snd a + snd b))
    |> (fun (d,r) -> {Duration = (uint64 d)*1UL<ms>; Record = (uint64 r)*1UL<mm> })
    
let parse' lines =
    lines
    |> parseRaces
    |> combineRaces
    |> calculateWaysToBeatRaceRecord
    
sample.SplitOnNewLine() |> parse'
Files[6] |> parse'