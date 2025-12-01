#load "common.fsx"
open System

type Direction =
    | LEFT
    | RIGHT

type Rotation = { Direction: Direction; Clicks: int }

let inline (%!) a b = (a % b + b) % b

let parseDirection (value: char) =
    match value with
    | 'R' -> RIGHT
    | 'L' -> LEFT
    | _ -> failwith "could not parse direction"

let parseRotation (value: string) =
    let direction = value |> Seq.head |> parseDirection
    let clicks = value |> Seq.tail |> Seq.map string |> String.concat "" |> int

    { Direction = direction
      Clicks = clicks }

let sample1 =
    """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"""

type State =
    { Position: int
      Part1: int
      Part2: int }

let foldForPassword state rotation =
    let rotations = rotation.Clicks / 100
    let steps = rotation.Clicks %! 100;

    let computePosition =
        match rotation.Direction with
        | LEFT -> state.Position - steps
        | RIGHT -> state.Position + steps

    match computePosition with
    | 0 |100| -100 ->
        { Position = 0
          Part1 = state.Part1 + 1
          Part2 = state.Part2 + rotations + 1 }
    // if previous position was 0 we don't want to add one
    | x when x %!100 <> x && state.Position = 0->
        { state with
            Position = computePosition %! 100
            Part2 = state.Part2 + rotations}
    | x when x %!100 <> x ->
        { state with
            Position = computePosition %! 100
            Part2 = state.Part2 + rotations + 1 }
    | _ ->
        { state with
            Position = computePosition
            Part2 = state.Part2 + rotations }


let compute (data: string array) =
    data
    |> Seq.map parseRotation
    |> Seq.fold foldForPassword { Position = 50; Part1 = 0; Part2 = 0 }

let sample1Result = sample1.ByNewLine() |> compute
let part1 = Files[1] |> compute

//sample1.ByNewLine() |> Seq.map parseRotation |> Seq.take 3 |> Seq.fold (foldForPassword validateStatePart2) {Position=50;ZeroHit=0};;

