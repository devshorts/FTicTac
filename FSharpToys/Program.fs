open Player
open TicTacToeData
open Board


//=======================================
// define the players
//=======================================
    
// upcast the player objects to interfaces since unlike c# you can't call methods
// of an interface on an object implementing that interface without direct casting

let playerX = new Player.ComputerPlayer(Player.X) :> IPlayer
let playerO = new Player.ComputerPlayer(Player.O) :> IPlayer

let playersList = [playerX;playerO]

let startingBoard = Board.createBoard 3

ignore(Board.playGame startingBoard playersList)