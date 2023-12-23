#load "Common.fsx"
    
open System.Collections.Generic

type Node = {
    Name: string
    NextNodeName: IReadOnlyDictionary<char, string>
}

type Network = {
    Nodes: IReadOnlyDictionary<string, Node>
}
    
let parseNetwork lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split '=' [name; Split ',' [Split '(' [left]; Split ')' [right]]] ->
            Some { Name=name; NextNodeName = readOnlyDict [ 'L', left; 'R', right ]   }
        | _ -> None
        )
    |> Array.choose id
    |> Array.map (fun n -> (n.Name, n))
    |> readOnlyDict
    |> (fun x -> {Nodes=x})
    
let countSteps (instructions:char array) startNode endNode (network:Network) =    
    let rec gotoEndNode (nodeName:string) index count =
        let index = if index >= instructions.Length then 0 else index
        match nodeName with
        | n when n = endNode -> count
        | _ -> gotoEndNode network.Nodes[nodeName].NextNodeName[instructions[index]] (index+1) (count+1)
        
    gotoEndNode startNode 0 0
    
let sample =
    """LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"""

let parse (lines:string array) =
    let instructions = lines |> Array.head |> _.ToCharArray()
    
    lines
    |> parseNetwork
    |> countSteps instructions "AAA" "ZZZ"

sample.SplitOnNewLine() |> parse
Files[8] |> parse

// Part 2

let sample2 =
    """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"""

// Function to calculate the GCD
let rec gcd a b =
    if b = 0UL then a
    else gcd b (a % b)

// Function to calculate the LCM of two numbers
let lcm a b = (a * b) / gcd a b

// Function to calculate the LCM of an array of numbers
let lcmArray arr =
    arr |> Array.fold lcm 1UL

let countSteps' (instructions:char array) (network:Network) =    
    let rec gotoEndingInZ (nodeName:string) index (stepCount:uint64) =            
        if stepCount % 1000000UL = 0UL then
            printfn $"%i{stepCount}"
            
        let index = if index >= instructions.Length then 0 else index
        match nodeName[2]='Z' with
        | true -> stepCount
        | _ ->
            gotoEndingInZ network.Nodes[nodeName].NextNodeName[instructions[index]] (index+1) (stepCount+1UL)
        
    network.Nodes.Keys
    |> Seq.filter (fun x -> x[2]='A')
    |> Seq.toArray
    |> Array.Parallel.map (fun node -> gotoEndingInZ node 0 0UL)
    |> lcmArray

let parse' (lines:string array) =
    let instructions = lines |> Array.head |> _.ToCharArray()
    
    lines
    |> parseNetwork
    |> countSteps' instructions

sample2.SplitOnNewLine() |> parse'
Files[8] |> parse'

