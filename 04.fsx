#load "common.fsx"
open System

let sample1 =
    """..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."""

type GridPosition =
    | Paper
    | Nothing

let parsePosition value =
    match value with
    | '@' -> Paper
    | '.' -> Nothing
    | x -> failwithf "Character %c could not be parsed" x


let parse (values: string array) =
    let sizeY = values |> Array.length
    let sizeX = values |> Array.head |> String.length

    let grid =
        values
        |> Array.mapi (fun y line -> line |> Seq.mapi (fun x value -> ((x, y), value |> parsePosition)))
        |> Seq.concat
        |> Map.ofSeq

    ((sizeX, sizeY), grid)

let isOnGrid (sizeX, sizeY) position =
    match position with
    | (x, _) when x < 0 -> false
    | (x, _) when x >= sizeX -> false
    | (_, y) when y < 0 -> false
    | (_, y) when y >= sizeY -> false
    | _ -> true

let getAdjacent size (x, y) =
    [ (x - 1, y - 1)
      (x, y - 1)
      (x + 1, y - 1)
      (x - 1, y)
      (x + 1, y)
      (x - 1, y + 1)
      (x, y + 1)
      (x + 1, y + 1) ]
    |> List.filter (isOnGrid size)

let compute (size, grid) =
    grid
    |> Map.toSeq
    |> Seq.filter (fun (position, value) -> value = Paper)
    |> Seq.map (fun (position, _) ->
        position
        |> getAdjacent size
        |> Seq.map (fun x -> grid |> Map.tryFind x)
        |> Seq.filter (fun x -> x = Some Paper))
    |> Seq.filter (fun x -> x |> Seq.length < 4)
    |> Seq.length




let samplePart1 = sample1.ByNewLine() |> parse |> compute
let part1 = Files[4] |> parse |> compute

let changeTo changeValue value=
    match value with
    |Some _ -> Some changeValue
    |None -> None
let rec compute2 (size, grid) =
    let possibleToRemove =
        grid
        |> Map.toSeq
        |> Seq.filter (fun (position, value) -> value = Paper)
        |> Seq.map (fun (position, _) ->
            let getAdjacentStack =
                position
                |> getAdjacent size
                |> Seq.map (fun x -> grid |> Map.tryFind x)
                |> Seq.filter (fun x -> x = Some Paper)
                |> Seq.choose id
            (position, getAdjacentStack))
        |> Seq.filter (fun (_,x) -> x |> Seq.length < 4)
    let removedNow = possibleToRemove |> Seq.length
    if removedNow = 0 then
        0
    else
        let newGrid =
            possibleToRemove
            |> Seq.fold (fun gridMap (position,_)->
                gridMap
                |> Map.change position (changeTo Nothing)) grid
        removedNow + compute2 (size, newGrid)

let samplePart2 = sample1.ByNewLine() |> parse |> compute2
let part2 = Files[4] |> parse |> compute2
