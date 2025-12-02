#load "common.fsx"
open System

let sample1 = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

let createSeq (range: string array) =
    let start = range.[0] |> int64
    let endValue = range.[1] |> int64
    Seq.init ((endValue-start+1L)|>int) (fun x -> start + (x|>int64))

let splitHalf (s: string) =
    let mid = s.Length / 2
    let first = s.Substring(0, mid)
    let second = s.Substring(mid)
    first, second

let validatePart1 (numberValue: int64) =
    let value = numberValue |> string
    if value.Length%2=1 then
        None
    else
        let first, second = splitHalf value
        if first = second then
            Some (value|>int64)
        else
            None

let compute validate (value: string) =
    let splitFn = (fun (x: string) -> x.Split("-"))
    value.Split(",")
    |> Seq.collect (splitFn>>createSeq)
    |> Seq.choose validate
    |> Seq.sumBy id


let sample1Result = sample1 |> compute validatePart1
let part1 = Files[2] |> Array.head |> compute validatePart1

let splitToParts (value: string) patternSize=
    if value.Length % patternSize <> 0 then
        None
    else
        [0..value.Length/patternSize-1]
        |> List.map (fun x -> value.Substring(x*patternSize, patternSize))
        |> Some
let validatePart2 (numberValue: int64) =
    let validatePattern values =
        let start::rest =values
        rest |> List.forall (fun x -> x=start)
    let value = numberValue |> string
    [1..value.Length/2]
    |> List.choose (splitToParts value)
    |> List.map validatePattern
    |> List.exists id
    |> (fun x -> if x then Some numberValue else None)

let sampleResultPart2 = sample1 |> compute validatePart2
let part2 = Files[2] |> Array.head |> compute validatePart2