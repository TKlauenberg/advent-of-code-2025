#load "common.fsx"

open System

let sample1 =
    "123 328  51 64 \n 45 64  387 23 \n  6 98  215 314\n*   +   *   +  "

type Operation =
    | Addition
    | Multiplication

type MathValue =
    | Number of uint64
    | Operation of Operation

let getOperation =
    function
    | Operation Addition -> (fun (a: uint64) (b: uint64) -> a + b)
    | Operation Multiplication -> (*)
    | Number _ -> failwith "Number not allowed"

let getValue =
    function
    | Number x -> x
    | Operation _ -> failwith "Operation not allowed"

let parseValue =
    function
    | "*" -> Operation Multiplication
    | "+" -> Operation Addition
    | x -> x |> uint64 |> Number

let parse (values: string array) =
    values
    |> Array.map (fun x ->
        x.Split(" ")
        |> Array.filter (fun x -> x <> "")
        |> Array.map parseValue
        |> Array.toList)
    |> Array.toList

let computeColumn column =
    let operation :: values = column |> List.rev
    let firstValues :: rest = values |> List.map getValue
    rest |> List.fold (operation |> getOperation) firstValues

let compute values =
    values |> List.transpose |> List.sumBy computeColumn



let samplePart1 = sample1.ByNewLine() |> parse |> compute
let part1 = Files[6] |> parse |> compute

let compute2 (values: string array) =
    let revert: string array = values |> Array.rev

    let operations =
        revert
        |> Array.head
        |> Seq.mapi (fun i x ->
            if x = ' ' then
                None
            else
                Some(i, x |> string |> parseValue |> getOperation))
        |> Seq.choose id

    let splitPositions =
        operations |> Seq.map fst |> Seq.toList |> (fun x -> x @ [ revert.[0].Length ])

    let splitWithLength =
        splitPositions
        |> List.take ((List.length splitPositions) - 1)
        |> List.zip (splitPositions |> List.tail)
        |> List.map (fun (endi, start) -> (start, endi - start))

    revert
    |> Array.tail
    |> Array.map (fun x ->
        splitWithLength
        |> Seq.map (fun (startIndex, length) -> x.Substring(startIndex, length)))
    |> Seq.transpose
    |> Seq.map (Seq.transpose >> (Seq.map (fun x -> x |> Seq.filter (fun c -> c<> ' ')|> Seq.rev |>String.Concat))>>Seq.filter (fun x -> x <> "")>> Seq.map uint64)
    |> Seq.zip (operations |> Seq.map snd)
    |> Seq.sumBy (fun (op, numbers) -> numbers |> Seq.tail |> Seq.fold op (numbers |> Seq.head))

let samplePart2 = sample1.ByNewLine() |> compute2
let part2 = Files[6] |> compute2