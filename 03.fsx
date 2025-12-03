#load "common.fsx"
open System

let sample1 =
    """987654321111111
811111111111119
234234234234278
818181911112111"""

type Bank = int array

let parseBank (value: string) : Bank =
    value.ToCharArray() |> Array.map (fun x -> x |> string |> int)

let parse (values: string array) = values |> Array.map parseBank

let foldForMaxValue (x, i) (y, i2) = if y >= x then (y, i2) else (x, i)

let getJolt (bank: Bank) =
    let indexed = bank |> Array.mapi (fun i x -> (x, i))

    let firstJolt =
        indexed |> Array.rev |> Array.tail |> Array.fold foldForMaxValue (0, 0)

    let secondJolt =
        indexed |> Array.skip ((snd firstJolt) + 1) |> Array.fold foldForMaxValue (0, 0)

    (fst firstJolt) * 10 + (fst secondJolt)



let compute (values: Bank array) = values |> Array.sumBy getJolt


let sample1Result = sample1.ByNewLine() |> parse |> compute
let part1 = Files[3] |> parse |> compute

let rec getJolt2 amount (bank: Bank) =


    let (jolt, index) =
        bank
        |> Array.mapi (fun i x -> (x, i))
        |> Array.rev
        |> Array.skip (amount - 1)
        |> Array.fold foldForMaxValue (0, 0)

    if amount > 1 then
        (jolt |> int64) * (pown 10L (amount - 1))
        + (getJolt2 (amount - 1) (bank |> Array.skip (index + 1)))
    else
        jolt |> int64

let sampleResultPart2 = sample1.ByNewLine() |> parse |> Array.sumBy (getJolt2 12)
let part2 = Files[3] |> parse |> Array.sumBy (getJolt2 12)
