#load "common.fsx"

open System

let sample1 =
    """3-5
10-14
16-20
12-18

1
5
8
11
17
32"""

let parseFreshIds (value: string) =
    let parsed = value.Split("-") |> Array.map uint64
    (parsed.[0], parsed.[1])

let parse (value: string array) =
    let freshIds = value |> Seq.takeWhile (fun x -> x <> "") |> Seq.map parseFreshIds

    let ingredients =
        value |> Seq.skipWhile (fun x -> x <> "") |> Seq.tail |> Seq.map uint64

    (ingredients, freshIds)

let isFresh freshIds ingredient =
    freshIds
    |> Seq.exists (fun (startValue, endValue) -> ingredient >= startValue && ingredient <= endValue)

let compute (ingredients, freshIds) =
    ingredients |> Seq.filter (isFresh freshIds) |> Seq.length

let samplePart1 = sample1.ByNewLine() |> parse |> compute
let part1 = Files[5] |> parse |> compute
// 442 too low because fresh/not fresh

let rec mergeRanges acc ranges =
    match ranges with
    | [] -> acc
    | (l1,h1)::(l2,h2)::t when l2 <= h1 + 1UL -> mergeRanges acc ([(min l1 l2,max h1 h2)] @ t)
    | h::t -> mergeRanges (acc@[h]) t
let countIds (startValue, endValue) = if startValue = 0UL && endValue = 0UL then 0UL else endValue - startValue + 1UL
let compute2 (_, freshIds) =
    freshIds
    |> Seq.toList
    |> List.sortBy fst
    |> mergeRanges []
    |> List.sumBy countIds

let samplePart2 = sample1.ByNewLine() |> parse |> compute2
let part2 = Files[5] |> parse |> compute2
// first version didn't include that IDs can be multiple
// second version 351715456920896UL too high
//351715456920870 still too high
//345336279418174UL wrong
//357452936910890UL
//344813017450467UL
//1335298770323015UL
//344813017450467UL
