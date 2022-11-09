open XO
open XO.Model
open XO.Utils
// For more information see https://aka.ms/fsharp-console-apps

let game = Game.create

let updateGame = Game.update game (Position (1,2))

updateGame.iter  View.render 


