module XO.AI

open Model

module AI =

    let private getPossibleMoves (board: Board.Board) =
        board.Value
        |> List.indexed
        |> List.filter (fun (_, elem) -> elem = Empty)
        |> List.map (fun (i, _) -> Move.fromIndex i)
        |> List.map (fun moveResult ->
            match moveResult with
            | Ok a -> a
            | Error _ -> failwith "")


    type private AIPlayer =
        | Naught
        | Cross
        member this.Next =
            match this with
            | Naught -> Cross
            | Cross -> Naught


    let private unwrapOrFail result =
        match result with
        | Ok a -> a
        | Error _ -> failwith ""

    let private minMax (game: Game) recurse returnMinOrMax : int =
        let possibleMoves =
            getPossibleMoves game.board

        let createGameWithMove move = move |> Game.update game

        possibleMoves
        |> List.map createGameWithMove
        |> List.map unwrapOrFail // ai will currently not cause failure (due to double move)
        |> List.map recurse
        |> returnMinOrMax



    // todo remove mutable values
    let private rateMove (game: Game) =
        let rec recurse (currentPlayer: AIPlayer) game =
            match game with
            | { state = Won Player.Naught } -> -1
            | { state = Won Player.Cross } -> 1
            | { state = Over _ } -> 0
            | _ ->
                match currentPlayer with
                | Naught ->
                    let mutable value = 2
                    value <- min (minMax game (recurse currentPlayer.Next) List.min) value
                    value
                | Cross ->
                    let mutable value = -2
                    value <- max (minMax game (recurse currentPlayer.Next) List.max) value
                    value
        
        recurse Naught game


    let miniMax (game: Game) =
        let possibleMoves =
            getPossibleMoves game.board

        possibleMoves
        |> List.map (Game.update game)
        |> List.map unwrapOrFail
        |> List.map rateMove
        |> List.zip possibleMoves
        |> List.maxBy snd
        |> fst
