open System
open System.IO
open System.Text.RegularExpressions

type Number =
    | one = 1
    | two = 2
    | three = 3
    | four = 4
    | five = 5
    | six = 6
    | seven = 7
    | eight = 8
    | nine = 9

let (|NumberAsText|_|) (str: string) =
   match Enum.TryParse(typeof<Number>, str) with
   | (true, e) -> Some (e :?> Number |> int |> string)
   | _ -> None
   
let (|Digit|_|) (str: string) =
    match Int32.TryParse str with
    | (true, v) -> Some str
    | _ -> None
    
let toString s full =
    match s with
    | NumberAsText v -> v
    | Digit v -> v
    | _ -> full
    
let regexPatterns =
    [1 .. 9]
    |> Seq.map enum<Number>
    |> Seq.map (_.ToString())
    |> Seq.append ["[0-9]"]
    
let matches patterns str =
    patterns
    |> Seq.map (fun p -> Regex.Matches(str, p))
    |> Seq.collect (fun matches -> [ for m in matches -> (m.Index, m.Value) ])
    |> Seq.sortBy fst
    |> Seq.toArray

let calibrationValue str =
    let values = matches regexPatterns str
    let first = snd (Array.head values)
    let second = snd (Array.last values)
    let v = (toString first str) + (toString second str)
    int v

let processFile filepath =
    File.ReadAllLines filepath
    |> Seq.map calibrationValue
    |> Seq.sum
    
let test = [|"two1nine";"eightwothree";"abcone2threexyz";"xtwone3four";"4nineeightseven2";"zoneight234";"7pqrstsixteen"|]
test |> Seq.map calibrationValue |> Seq.sum

let file = "data/day1.txt"
processFile file