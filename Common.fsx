[<AutoOpen>]
module Common
 
open System
open type System.Environment

type String with

    member this.SplitOnNewLine() =
        this.Split(Environment.NewLine, StringSplitOptions.None)
        
type Files() =
    member _.Item
        with get file = System.IO.File.ReadAllLines $"data/day%i{file}.txt"

/// Provides access to data files using an indexer e.g. Files[1] gets the path to the Day One data file.
let Files = Files()

let (|Split|) (on: char) (s: string) =
    s.Split(on, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    |> Array.toList
    
let (|Chars|) (s: string) =
    s.ToCharArray()
    |> Array.toList
    
let (|Int|_|) (s: string) =
    match Int32.TryParse s with
    | true, n -> Some n
    | false, _ -> None
    
let (|UInt64|_|) (s: string) =
    match UInt64.TryParse s with
    | true, n -> Some n
    | false, _ -> None
