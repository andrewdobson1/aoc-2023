open System.IO
open System.Text.RegularExpressions

type RGB =
    { Red: int
      Green: int
      Blue: int }
    
type Game =
    { Id: int
      RGBs: RGB seq }

let gameId s =
    let m = Regex.Match(s, "Game (\d+):")
    int m.Groups[1].Value

let blue = "(\d+) blue"
let red = "(\d+) red"
let green = "(\d+) green"

let sum regex s =
    Regex.Matches(s, regex)
    |> Seq.map (_.Groups[1].Value >> int)
    |> Seq.sum
   
let count regex s =
    let m = Regex.Match(s, regex)
    if m.Success then
        int m.Groups[1].Value
    else 0
   
let toRGB s =
    { Red=(count red s); Green = (count green s); Blue = (count blue s) } 
   
let toGame (s:string) =
    { Id=(gameId s)
      RGBs = s.Split(";") |> Seq.map toRGB }
                 
let isGameValid (validRgb : RGB) (game : Game) =
    game.RGBs
    |> Seq.forall (fun rgb -> 
        rgb.Red <= validRgb.Red &&
        rgb.Green <= validRgb.Green &&
        rgb.Blue <= validRgb.Blue)

let validRGB = { Red = 12; Green = 13; Blue = 14 }

let processFile filepath =
    File.ReadAllLines filepath
    |> Seq.map toGame
    |> Seq.map (fun game ->
        if (isGameValid validRGB game) then game.Id else 0)
    |> Seq.sum

let file = "data/day2.txt"
processFile file


// Part 2
let minRGB (game:Game) =
    ({Red=0;Green=0;Blue=0}, game.RGBs)
    ||> Seq.fold (fun acc rgb ->
        {Red = (max acc.Red rgb.Red)
         Green = (max acc.Green rgb.Green)
         Blue = (max acc.Blue rgb.Blue) })

let power (rgb:RGB) =
    rgb.Red * rgb.Green * rgb.Blue

let processFile2 filepath =
    File.ReadAllLines filepath
    |> Seq.map (toGame >> minRGB >> power)
    |> Seq.sum
    
processFile2 file