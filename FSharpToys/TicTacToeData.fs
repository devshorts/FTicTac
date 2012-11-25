module TicTacToeData

type Player = 
    | X
    | O

type CellElement =
    | Some of Player
    | None