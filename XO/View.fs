module XO.View

open Model
let square (xo: string) = $"| {xo} |"

let line = "---------------\n"

let render (game: Game) =
    let board = game.board
    let endIndices = [ 2; 5; 8 ]

    let boardStr =
        board
        |> List.map (fun xo -> square (xo |> string))
        |> List.indexed
        |> List.map (fun (i, elem) ->
            if endIndices |> List.contains i then
                elem + "\n" + line 
            else
                elem)

    boardStr |> List.map (fun x -> printf $"{x}")  |> ignore
