#load "Common.fsx"

type Card =
    | A = 14
    | K = 13
    | Q = 12
    | J = 11
    | T = 10
    
let (|Card|_|) (c:char) =    
    match string c with
    | "A" -> Some Card.A
    | "K" -> Some Card.K
    | "Q" -> Some Card.Q
    | "J" -> Some Card.J
    | "T" -> Some Card.T
    | Int n -> Some (enum<Card> n)
    | _ -> None
  
type HandType =
    | FiveOfAKind = 6
    | FourOfAKind = 5
    | FullHouse = 4
    | ThreeOfAKind = 3
    | TwoPair = 2
    | Pair = 1
    | HighCard = 0
    
let classify (hand:Card list) =
    match hand |> List.countBy id |> List.map snd |> List.sortDescending with
    | 5 :: _ -> HandType.FiveOfAKind
    | 4 :: _ -> HandType.FourOfAKind
    | 3 :: 2 :: _-> HandType.FullHouse
    | 3 :: _ -> HandType.ThreeOfAKind
    | 2 :: 2 :: _ -> HandType.TwoPair
    | 2 :: _ -> HandType.Pair
    | 1 :: _ -> HandType.HighCard
    | _ -> failwith "invalid hand"    
    
type Line = {
    Hand: Card list
    Bid: int
} with
    member this.Type = classify this.Hand
    
let compareByHighCard (arr1: Card list) (arr2: Card list) =
    let rec compareAtIndex i =
        if i >= arr1.Length || i >= arr2.Length then 0
        else
            match arr1.[i].CompareTo(arr2.[i]) with
            | 0 -> compareAtIndex (i + 1)
            | c -> c
    compareAtIndex 0

let compareByTypeThenHighCard (line1:Line) (line2:Line) =
    match (int line1.Type).CompareTo(int line2.Type) with
    | 0 -> compareByHighCard line1.Hand line2.Hand
    | c -> c
    
let sample =
    """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"""

let parse lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [ Chars [Card a; Card b; Card c; Card d; Card e]; Int bid] -> 
            Some {
                Hand = [ a; b; c; d; e ]
                Bid = bid
            }
        | _ -> None)
    |> Array.choose id
    |> Array.sortWith compareByTypeThenHighCard
    |> Array.indexed
    |> Array.map (fun (i, line) -> (i+1)*line.Bid)
    |> Array.sum

sample.SplitOnNewLine() |> parse
Files[7] |> parse

// Part 2
    
type Part2HandType = {
    Type: HandType
    HighCard: Card
}

type Part2Line = {
    Line: Line
    OrigHand: Card list
}

let classify' (hand:Card list) =
    match hand |> List.countBy id |> List.sortByDescending (fun (h,c) -> (c,h)) with
    | (c,5) :: _ -> {Type=HandType.FiveOfAKind; HighCard =c}
    | (c,4) :: _ -> {Type=HandType.FourOfAKind; HighCard = c} 
    | (c,3) :: (_,2) :: _-> {Type=HandType.FullHouse; HighCard = c} 
    | (c,3) :: _ -> {Type=HandType.ThreeOfAKind; HighCard = c} 
    | (c,2) :: (_,2) :: _ -> {Type=HandType.TwoPair; HighCard = c} 
    | (c,2) :: _ -> {Type=HandType.Pair; HighCard = c} 
    | (c,1) :: _ -> {Type=HandType.HighCard; HighCard = c}
    | _ -> {Type=HandType.HighCard; HighCard = enum<Card> 1}

let replaceJokers hand card =
    hand
    |> List.map(fun x -> if x=Card.J then card else x)
    
let replaceJokers' (line:Part2Line) =
    let handWithoutJokers = line.OrigHand |> List.filter (fun c -> c <> Card.J)
    let hand =
        match classify' handWithoutJokers with
        | {Type=HandType.FiveOfAKind;HighCard=c} -> replaceJokers line.OrigHand c
        | {Type=HandType.FourOfAKind;HighCard=c} -> replaceJokers line.OrigHand c
        | {Type=HandType.FullHouse;HighCard=c} -> replaceJokers line.OrigHand c
        | {Type=HandType.ThreeOfAKind;HighCard=c} -> replaceJokers line.OrigHand c
        | {Type=HandType.TwoPair;HighCard=c} -> replaceJokers line.OrigHand c
        | {Type=HandType.Pair;HighCard=c} -> replaceJokers line.OrigHand c
        | {Type=HandType.HighCard;HighCard=c} -> replaceJokers line.OrigHand c
        | _ -> failwith "replaceJokers': invalid hand"
        
    { line with Part2Line.Line.Hand = hand }

let compareByTypeThenHighCard' (line1:Part2Line) (line2:Part2Line) =
    match (int line1.Line.Type).CompareTo(int line2.Line.Type) with
    | 0 -> compareByHighCard
               (replaceJokers line1.OrigHand (enum<Card> 1) )
               (replaceJokers line2.OrigHand (enum<Card> 1))
    | c -> c
    
    
let parse' lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [ Chars [Card a; Card b; Card c; Card d; Card e]; Int bid] -> 
            Some {
                Line = {
                    Hand = [ a; b; c; d; e ]
                    Bid = bid
                }
                OrigHand = [ a; b; c; d; e ]
                
            }
        | _ -> None)
    |> Array.choose id
    |> Array.map replaceJokers'
    |> Array.sortWith compareByTypeThenHighCard'
    |> Array.indexed
    |> Array.map (fun (i, line) -> (i+1)*line.Line.Bid)
    |> Array.sum    

sample.SplitOnNewLine() |> parse'
Files[7] |> parse'