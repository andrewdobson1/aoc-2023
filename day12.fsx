#load "Common.fsx"

open System
open System.Collections.Generic
open System.Linq
        
let rec calculate (pattern:string,nums) (cache: Map<(string*int list), uint64>) =
    let key = (pattern,nums)
    match cache.TryGetValue(key) with
    | true, value -> (value, cache)
    | false, _ -> 
        let result, updatedCache =
            match pattern.FirstOrDefault() with
            | '.' -> calculate (pattern[1..],nums) cache
            | '?' ->
                let res1, cache1 = calculate ("." + pattern.[1..], nums) cache
                let res2, cache2 = calculate ("#" + pattern.[1..], nums) cache1
                (res1 + res2, cache2)
            | '#' ->
                match nums with
                | [] -> (0UL,cache)
                | damaged::nums ->
                    let potentiallyDamaged = pattern |> Seq.takeWhile (fun c -> c = '#' || c = '?') |> Seq.length
                    
                    if potentiallyDamaged < damaged then (0UL,cache)
                    else if pattern.Length = damaged then calculate ("",nums) cache
                    else if pattern[damaged] = '#' then (0UL,cache)
                    else calculate (pattern[(damaged+1)..],nums) cache
            | _ -> if nums.Length = 0 then (1UL,cache) else (0UL,cache)
        let finalCache = updatedCache.Add(key,result)
        (result,finalCache)      
        
let sample =
    """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"""

let parse lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [pattern; Split ',' nums] ->
            Some (pattern, nums |> List.map int)
        | _ -> None)
    |> Array.choose id
    |> Array.map (fun x -> calculate x Map.empty)
    |> Array.sumBy fst

sample.SplitOnNewLine() |> parse
Files[12] |> parse

// Part 2

let unfold (pattern,nums) =
    let pattern = String.Join('?', Enumerable.Repeat(pattern,5))
    let nums = String.Join(',', Enumerable.Repeat(nums,5))
    (pattern,nums)

let parse' lines =
    lines
    |> Array.map (fun line ->
        match line with
        | Split ' ' [pattern; nums] ->
            let pattern,nums = unfold (pattern,nums)
            Some (pattern, nums.Split(',') |> Array.toList |> List.map int)
        | _ -> None)
    |> Array.choose id
    |> Array.map (fun x -> calculate x Map.empty)
    |> Array.sumBy fst

sample.SplitOnNewLine() |> parse'
Files[12] |> parse'

