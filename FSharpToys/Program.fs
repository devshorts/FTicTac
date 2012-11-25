open Player
open TicTacToeData
open Board


//=======================================
// define the players
//=======================================
    
// upcast the player objects to interfaces since unlike c# you can't call methods
// of an interface on an object implementing that interface without direct casting

let humanPlayerX = new Player.HumanPlayer(Player.X) :> IPlayer
let humanPlayerO = new Player.HumanPlayer(Player.O) :> IPlayer

let playersList = [humanPlayerX;humanPlayerO]

let startingBoard = Board.createBoard 3

ignore(Board.playGame startingBoard playersList)