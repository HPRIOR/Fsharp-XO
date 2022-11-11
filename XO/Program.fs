open XO
open XO.Model
open AI

let game = Game.create


let getUserInput _ : Result<Move.Move, string> =
    try
        let input = System.Console.ReadLine()
        let split = input.Split [| ' ' |]
        let x, y = split[0] |> int, split[1] |> int
        Move.create (x, y)
    with
    | _ -> Error "Could not parse input, should be in the format 'x y'"


type Result<'a, 'b> with
    member this.AsOption =
        match this with
        | Ok a -> Some a
        | Error _ -> None
        
        


let rec getUserMove (currentGame: Game) (turn: Player) (previousMove: Move.Move option) =
    View.render currentGame

    match currentGame with
    | { state = Won player } -> printf $"{player} won!"
    | { state = Over msg } -> printf $"{msg}"
    | currentGame ->
        let move =
            if turn = Naught then
                getUserInput ()
            else
                Ok (AI.miniMax currentGame)

        let gameResult =
            move |> Result.bind (Game.update currentGame)

        match gameResult with
        | Error e ->
            printf $"{e}\n"
            getUserMove currentGame turn move.AsOption
        | Ok newGame -> getUserMove newGame turn.Next move.AsOption



[<EntryPoint>]
let main _ =
    getUserMove Game.create Naught None
    0
