open XO
open XO.Model
open XO.Utils
// For more information see https://aka.ms/fsharp-console-apps

let game = Game.create

let mutable loop = true

let parse (input: string) =
    try
        let split = input.Split [| ' ' |]
        Ok(split[0] |> int, split[1] |> int)
    with
    | :? System.FormatException -> Error "could not parse input"




while loop do
    let input = System.Console.ReadLine()
    let parseResult = parse input
    
    match parseResult with
    | 


let updateGame =
    Game.update game (Move.create (1, 2))

updateGame.iter View.render
