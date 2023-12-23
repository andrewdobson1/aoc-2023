#load "Common.fsx"

let sample =
    """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"""

let extrapolate (history:int array) =
    let rec loop xs acc =
        match xs with
        | [||] | [|_|] -> acc
        | _ when Array.forall (fun x -> x = 0) xs -> acc
        | _ ->
            loop [| for i in 0 .. xs.Length-2 do yield xs[i+1]-xs[i] |] (xs[xs.Length-1]::acc)
    
    loop history []
    |> List.sum
        
let parseHistory line =
    match line with
    | Split ' ' history -> history |> List.map int |> Array.ofList

let parse lines =
    lines
    |> Array.map parseHistory
    |> Array.map extrapolate
    |> Array.sum
    
    
sample.SplitOnNewLine() |> parse
Files[9] |> parse

// Part 2

let sample2 =
    """10 13 16 21 30 45
"""

let extrapolateBack history =
    let rec loop xs acc =
        match xs with
        | [||] | [|_|] -> acc
        | _ when Array.forall (fun x -> x = 0) xs -> acc
        | _ ->
            loop [| for i in 0 .. xs.Length-2 do yield xs[i+1]-xs[i] |] (xs[0]::acc)
    
    loop history []
    |> List.reduce (fun a b  -> b-a)
        

let parse' lines =
    lines
    |> Array.map parseHistory
    |> Array.filter (fun x-> x.Length > 0)
    |> Array.map extrapolateBack
    |> Array.sum

sample2.SplitOnNewLine() |> parse'
Files[9] |> parse'

