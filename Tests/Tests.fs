module Tests

open XO.Model
open XO.AI
open Xunit
open XO.Utils

[<Fact>]
let ``Game won when connection reached`` () =
    let initGame = Game.create

    let moves =
        [ (0, 0)
          (1, 0)
          (0, 1)
          (1, 1)
          (0, 2) ]
        |> List.map Move.create
        |> List.map (fun res ->
            match res with
            | Ok move -> move
            | Error _ -> failwith "")

    let game =
        moves
        |> List.fold
            (fun gamResult move ->
                gamResult
                |> Result.bind (fun game -> move |> Game.update game))
            (Ok initGame)

    let expectedState = Won Naught

    let expectedBoard: BoardPiece list =
        [ BoardPiece.Naught
          BoardPiece.Naught
          BoardPiece.Naught
          BoardPiece.Cross
          BoardPiece.Cross
          BoardPiece.Empty
          BoardPiece.Empty
          BoardPiece.Empty
          BoardPiece.Empty ]

    match game with
    | Error _ -> failwith "game update failed"
    | Ok g ->

        g.board.Value
        |> List.zip expectedBoard
        |> List.iter Assert.Equal

        Assert.Equal(expectedState, g.state)


[<Fact>]
let ``Error returned when move made on occupied tile`` () =
    let initGame = Game.create

    let moves =
        [ (0, 0); (0, 0) ]
        |> List.map Move.create
        |> List.map (fun res ->
            match res with
            | Ok move -> move
            | Error _ -> failwith "")

    let game =
        moves
        |> List.fold
            (fun gamResult move ->
                gamResult
                |> Result.bind (fun game -> move |> Game.update game))
            (Ok initGame)

    match game with
    | Error _ -> Assert.True |> ignore
    | Ok _ -> failwith ""


[<Fact>]
let ``Game can be won on last move`` () =
    let initGame = Game.create

    let moves =
        [ (0, 0)
          (0, 1)
          (0, 2)
          (1, 0)
          (1, 1)
          (1, 2)
          (2, 1)
          (2, 0)
          (2, 2) ]
        |> List.map Move.create
        |> List.map (fun res ->
            match res with
            | Ok move -> move
            | Error _ -> failwith "")

    let game =
        moves
        |> List.fold
            (fun gamResult move ->
                gamResult
                |> Result.bind (fun game -> move |> Game.update game))
            (Ok initGame)

    let expectedState = Won Naught

    let expectedBoard: BoardPiece list =
        [ BoardPiece.Naught
          BoardPiece.Cross
          BoardPiece.Naught
          BoardPiece.Cross
          BoardPiece.Naught
          BoardPiece.Cross
          BoardPiece.Cross
          BoardPiece.Naught
          BoardPiece.Naught ]



    match game with
    | Error _ -> failwith "game update failed"
    | Ok g ->

        g.board.Value
        |> List.zip expectedBoard
        |> List.iter Assert.Equal

        Assert.Equal(expectedState, g.state)


[<Fact>]
let ``Game over when game full`` () =
    let initGame = Game.create

    let moves =
        [ (0, 0)
          (1, 1)
          (2, 0)
          (1, 0)
          (1, 2)
          (2, 1)
          (0, 1)
          (0, 2)
          (2, 2) ]
        |> List.map Move.create
        |> List.map (fun res ->
            match res with
            | Ok move -> move
            | Error _ -> failwith "")

    let game =
        moves
        |> List.fold
            (fun gamResult move ->
                gamResult
                |> Result.bind (fun game -> move |> Game.update game))
            (Ok initGame)

    let expectedState = Over "Nobody won :("

    let expectedBoard: BoardPiece list =
        [ BoardPiece.Naught
          BoardPiece.Naught
          BoardPiece.Cross
          BoardPiece.Cross
          BoardPiece.Cross
          BoardPiece.Naught
          BoardPiece.Naught
          BoardPiece.Cross
          BoardPiece.Naught ]



    match game with
    | Error _ -> failwith "game update failed"
    | Ok g ->

        g.board.Value
        |> List.zip expectedBoard
        |> List.iter Assert.Equal

        Assert.Equal(expectedState, g.state)

[<Fact>]
let ``Ai will find correct move`` () =
    let initGame = Game.create

    let moves =
        [ (0, 0); (1, 1); (2, 0) ]
        |> List.map Move.create
        |> List.map (fun res ->
            match res with
            | Ok move -> move
            | Error _ -> failwith "")

    let game =
        match moves
              |> List.fold
                  (fun gamResult move ->
                      gamResult
                      |> Result.bind (fun game -> move |> Game.update game))
                  (Ok initGame)
            with
        | Ok g -> g
        | Error _ -> failwith "Could not execute moves"



    let lastMadeMove =
        match Move.create (2, 0) with
        | Ok move -> move
        | Error _ -> failwith "could not create move"

    let expectedMove =
        match Move.create (1, 0) with
        | Ok move -> move
        | Error _ -> failwith "could not create move"


    let aiMove = AI.miniMax game

    Assert.Equal(expectedMove, aiMove)


    ()
