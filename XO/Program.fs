open XO
open XO.Model

let game = Game.create


let getUserInput _ : Result<int * int, string> =
    try
        let input = System.Console.ReadLine()
        let split = input.Split [| ' ' |]
        let x, y = split[0] |> int, split[1] |> int
        Ok(x, y)
    with
    | _ -> Error "Could not parse input, should be in the format 'x y'"



let rec getUserMove (currentGame: Game) =
    View.render currentGame

    match currentGame with
    | { state = Won player } -> printf $"{player} won!"
    | { state = Over msg } -> printf $"{msg}"
    | currentGame ->
        let gameResult =
            getUserInput ()
            |> Result.bind Move.create
            |> Result.bind (Game.update currentGame)

        match gameResult with
        | Error e ->
            printf $"{e}\n"
            getUserMove currentGame
        | Ok newGame -> getUserMove newGame



[<EntryPoint>]
let main _ =
    getUserMove Game.create
    0
