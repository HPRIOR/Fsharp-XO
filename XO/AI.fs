module XO.AI

open Model

module AI =

    let possibleMoves (board: Board.Board) =
        board.Value
        |> List.indexed
        |> List.filter (fun (_, elem) -> elem = Empty)
        |> List.map (fun (i, _) -> Move.fromIndex i)
        |> List.map (fun moveResult ->
            match moveResult with
            | Ok a -> a
            | Error _ -> failwith "")


    type AIPlayer =
        | Naught
        | Cross
        member this.Next =
            match this with
            | Naught -> Cross
            | Cross -> Naught


    let unwrapOrFail result =
        match result with
        | Ok a -> a
        | Error _ -> failwith ""


    let selectMove (game: Game) firstMove =
        let rec recurse (currentPlayer: AIPlayer) currentMove game =
            match game with
            | { state = Won Player.Naught } -> (-1, currentMove)
            | { state = Won Player.Cross } -> (1, currentMove)
            | { state = Over _ } -> (0, currentMove)
            | _ ->
                match currentPlayer with
                | Naught ->
                    possibleMoves game.board
                    |> List.map (fun nextMove -> ((Game.update game nextMove), nextMove))
                    |> List.map (fun (nextGame, nextMove) -> ((unwrapOrFail nextGame), nextMove))
                    |> List.map (fun (nextGame, nextMove) -> recurse currentPlayer.Next nextMove nextGame)
                    |> List.min
                | Cross ->
                    possibleMoves game.board
                    |> List.map (fun nextMove -> ((Game.update game nextMove), nextMove))
                    |> List.map (fun (nextGame, nextMove) -> ((unwrapOrFail nextGame), nextMove))
                    |> List.map (fun (nextGame, nextMove) -> recurse currentPlayer.Next nextMove nextGame)
                    |> List.max

        recurse Naught firstMove game |> snd

// return the best score for each possible move
