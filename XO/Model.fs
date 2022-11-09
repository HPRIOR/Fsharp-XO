module XO.Model

type BoardPiece =
    | Naught
    | Cross
    | Empty

    override this.ToString() =
        match this with
        | Naught -> "0"
        | Cross -> "X"
        | Empty -> " "


type Player =
    | Naught
    | Cross
    member this.Next =
        match this with
        | Naught -> Cross
        | Cross -> Naught

    member this.AsBoardPiece =
        match this with
        | Naught -> BoardPiece.Naught
        | Cross -> BoardPiece.Cross

type State =
    | Active of CurrentPlayer: Player
    | Won of Winner: Player
    member this.CurrentPlayer: Player =
        match this with
        | Active player -> player
        | Won player -> player




module Move =

    // type is private and value can only be access through Value member
    type Move =
        private
        | Position of int
        member this.Value =
            match this with
            | Position a -> a

    // Make invalid states unrepresentable...
    let create (position: int * int) : Move option =
        let x, y = position

        if x >= 3 || y >= 3 || x < 0 || y < 0 then
            None
        else
            Some(Position((x * 3) + y))


module Board =
    type Board =
        private
        | Board of BoardPiece list
        member this.Value =
            match this with
            | Board b -> b

    let create =
        Board [ for _ in 0..8 -> Empty ]

    let updateWithMove (move: Move.Move) (piece: BoardPiece) (board: Board) =
        Board(board.Value |> List.updateAt move.Value piece)

    let private boardConnections =
        [ (0, 1, 2)
          (3, 4, 5)
          (6, 7, 8)
          (0, 3, 6)
          (1, 4, 7)
          (2, 5, 8)
          (0, 4, 8)
          (6, 4, 2) ]

    let private boardPieceConnections (board: BoardPiece list) =
        boardConnections
        |> List.map (fun (x, y, z) -> [ board[x]; board[y]; board[z] ])

    let private emptyConnections pieceList = not (pieceList |> List.contains Empty)

    let private completeConnection pieceList =
        List.length (pieceList |> List.distinct) = 1

    let private hasConnectedBoardPieces pieceConn =
        pieceConn
        |> List.filter emptyConnections
        |> List.exists completeConnection

    let hasWinner (board: Board) =
        board.Value
        |> boardPieceConnections
        |> hasConnectedBoardPieces

    let occupiedAt (move: Move.Move) (board: Board) =
        let board = board.Value
        let move = move.Value
        board[move] <> Empty



type Game = { state: State; board: Board.Board }


module Game =
    let create =
        { state = Active Player.Naught
          board = Board.create }


    let update (game: Game) (move: Move.Move) : Result<Game, Game> =
        let board = game.board

        if board |> Board.occupiedAt move then
            Error game
        else
            let state = game.state

            let newBoard =
                board
                |> Board.updateWithMove move state.CurrentPlayer.AsBoardPiece

            let newState =
                match newBoard |> Board.hasWinner with
                | true -> Won state.CurrentPlayer
                | false -> Active state.CurrentPlayer.Next

            Ok { state = newState; board = newBoard }
