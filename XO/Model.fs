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

type BoardPiece with
    member this.AsPlayer =
        match this with
        | Naught -> Player.Naught
        | Cross -> Player.Cross
        | _ -> failwith "Cannot convert empty to player"

type State =
    | Active of CurrentPlayer: Player
    | Won of Winner: Player
    | Over of Msg: string
    member this.CurrentPlayer: Player =
        match this with
        | Active player -> player
        | Won player -> player
        | _ -> Naught




module Move =

    // type is private and value can only be access through Value member
    type Move =
        private
        | Position of int
        member this.Value =
            match this with
            | Position a -> a

    // Make invalid states unrepresentable...
    let private boardErrorMsg =
        "Move must be within the bounds of the board"

    let create (position: int * int) : Result<Move, string> =
        let x, y = position

        if x >= 3 || y >= 3 || x < 0 || y < 0 then
            Error boardErrorMsg
        else
            Ok(Position((x * 3) + y))

    let fromIndex (i: int) : Result<Move, string> =
        if i > 8 then
            Error boardErrorMsg
        else
            Ok(Position i)



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

    let (|IsOccupied|_|) (move: Move.Move) (board: Board) =
        if board |> occupiedAt move then
            Some "Move already occupied\n"
        else
            None


    let isFull (board: Board) =
        not (board.Value |> (List.exists (fun x -> x = Empty)))




type Game = { state: State; board: Board.Board }


module Game =
    let create =
        { state = Active Player.Naught
          board = Board.create }


    let update (game: Game) (move: Move.Move) : Result<Game, string> =
        let board = game.board
        let state = game.state
        let currentPlayer = state.CurrentPlayer

        let currentBoardPiece =
            currentPlayer.AsBoardPiece

        match board with
        | Board.IsOccupied move msg -> Error msg
        | _ ->

            let newBoard =
                board
                |> Board.updateWithMove move currentBoardPiece

            let hasWinner = newBoard |> Board.hasWinner
            let isFull = newBoard |> Board.isFull

            let newState =
                match hasWinner with
                | true -> Won currentPlayer
                | false -> Active currentPlayer.Next

            if isFull && not hasWinner then
                Ok
                    { state = Over "Nobody won :("
                      board = newBoard }
            else
                Ok { state = newState; board = newBoard }
