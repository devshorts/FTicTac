module Player

open TicTacToeData

exception InvalidPlayer of string

//---------------------------------
// player information
//---------------------------------

let tokenType player = 
    match player with 
        | Player.X -> "x"
        | Player.O -> "o"

type IPlayer =
    abstract member play : 'a list list -> (Player * (int * int))

type HumanPlayer (token:Player) =

    let playerToken = token

    //=======================================
    // input
    //=======================================

    member private this.readInPosition boardSize = 
        let safeConvert (number:string) = 
            try
                (System.Convert.ToInt32 number)
            with
                | _ -> -1

        let getPosFunc = fun (label : string) -> 
                            System.Console.Write("{0}: ", label)
                            let mutable item = System.Console.ReadLine()
                        
                            let validMove = fun x -> let num = safeConvert x
                                                     num >= 0 && num < boardSize
                        
                            while validMove item = false do
                                System.Console.WriteLine("Enter a valid move (0 to {0})", boardSize - 1)
                                item <- System.Console.ReadLine()

                            System.Convert.ToInt32(item)

        (getPosFunc "row", getPosFunc "col")

    interface IPlayer with
        member this.play board = 
            System.Console.WriteLine("Player {0} turn", tokenType playerToken)
            (playerToken, this.readInPosition (List.length board))
        