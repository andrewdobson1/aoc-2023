#load "Common.fsx"

open System
open System.IO
open System.Text.RegularExpressions

type ScratchCard = {
    CardId: int
    WinningNumbers: int []
    Numbers: int []
}

let toScratchCard s =
    let cardId = int Regex.Match(s, "Card\s+(\d+):").Groups[1].Value
    
    let winningNumbers =
        Regex.Match(s, ":(.+)[|]").Groups[1].Value.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toArray
    
    let numbers =
        Regex.Match(s, "[|](.+)").Groups[1].Value.Split(" ", StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map int
        |> Seq.toArray        
    
    { CardId=cardId; WinningNumbers = winningNumbers; Numbers = numbers }


let winningCount card =
    card.Numbers
    |> Array.filter (fun n -> card.WinningNumbers |> Array.contains n)
    |> Array.length      

let cardResult (card:ScratchCard) =
    let winningCount = winningCount card

    if winningCount <= 1 then
        winningCount
    else
        let power = winningCount - 1
        (int (Math.Pow(2.0, power)))
        
let cardResultTotal cards =
    cards
    |> Array.map cardResult
    |> Array.sum
        
let parse lines =
    lines
    |> Array.map toScratchCard

let sample =
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""

let file = "AdventOfCode2023/data/day4.txt"
let parsedSample = sample.SplitOnNewLine() |> parse 
let parsedFile = File.ReadAllLines(file) |> parse 

parsedSample |> cardResultTotal
parsedFile |> cardResultTotal

// Part 2

let calculateTotal (cards:ScratchCard[]) lst =
    let rec loop acc = function
        | [] -> acc
        | x :: xs ->
            match winningCount x with
            | 0 -> loop (x :: acc) xs
            | count ->
                let wonCards = Array.sub cards x.CardId count |> Array.toList
                let scratchCards = loop (x :: acc) wonCards
                loop scratchCards xs
    
    loop [] lst
    |> List.length

parsedSample |> (fun x -> calculateTotal x (Array.toList x))
parsedFile |> (fun x -> calculateTotal x (Array.toList x))
