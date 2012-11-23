exception DimensionException of string

type CellElement =
    | Some of string
    | None

let reverseList list = List.fold (fun acc elem -> elem::acc) [] list

//---------------------------------
// create the board
//---------------------------------

let rec createRow width = 
    if width = 0 then []
    else CellElement.None::createRow(width - 1)

let rec createBoardHelper dimension init = 
    if(init = 0) then []
    else createRow(dimension)::(createBoardHelper dimension (init-1))

let createBoard dimension = createBoardHelper dimension dimension

//---------------------------------
// get from board
//---------------------------------

let rec getRowElement row element = 
    if element = 0 then
        match row with
            | [] -> raise(DimensionException("Didn't find a row"))
            | h::_ -> h
    else 
        match row with
            | [] -> raise(DimensionException("Didn't find a row"))
            | h::t -> getRowElement t (element-1)

let getPosition board (row,col) = 
    let rowArray = getRowElement board row
    getRowElement rowArray col

//---------------------------------
// print board
//---------------------------------

let rec printRow row = 
    match row with 
        | [] -> System.Console.WriteLine()
        | h::t -> 
            match h with 
               | CellElement.Some(token) ->  System.Console.Write(token)
               | CellElement.None -> System.Console.Write(" ")
            System.Console.Write(" | ")
            printRow t


let rec printBoardHelper board = 
    match board with 
        | [] -> System.Console.WriteLine()
        | h::[] -> printRow h
        | h::t -> 
            printRow h
            printBoardHelper t

let printBoard board =
    printBoardHelper board
    System.Console.WriteLine()
    System.Console.WriteLine()


//---------------------------------
// set position on  board
//---------------------------------

let updateToken targetItem doUpdate currentItem = 
    if doUpdate() then 
        match currentItem with
            | CellElement.Some(i) -> System.Console.WriteLine("move invalid")
                                     currentItem
            | CellElement.None -> targetItem

    else currentItem

let rec setTokenHelper board (row, col) token iter = 
    if iter = row then
        let counter = fun x -> x + 1
        let add = ref 0
        let doUpdateFunction = fun () -> 
                                        let orig = !add
                                        add := counter !add 
                                        if orig = col then true
                                        else false

        let updateTokenFunc = updateToken (CellElement.Some(token)) doUpdateFunction

        let rowElement = getRowElement board iter

        (List.map updateTokenFunc rowElement)::(setTokenHelper board (row, col) token (iter-1))
    else
        if iter = -1 then []            
        else
            let rowElement = getRowElement board iter
            rowElement::(setTokenHelper board (row, col) token (iter-1))

let setToken board (row,col) token = reverseList (setTokenHelper board (row,col) token (board.Length-1))

//=======================================
// detect game over
//=======================================

let rowWin row = 
    let sequences = row |> Seq.countBy id |> Seq.toList

    let rec testSequences seq = 
        match seq with 
            | [] -> (false, CellElement.None)
            | h::t -> match h with 
                        | (key, count) -> if count = Seq.length row then (true, key)
                                          else testSequences t

    testSequences sequences
   
let rec gameOverRows board = 
    match board with
        | [] -> false
        | h::t -> let winner = rowWin h 
                  match winner with
                    | (won, CellElement.None) -> gameOverRows t
                    | (won, CellElement.Some(x)) -> 
                                             if won then 
                                                System.Console.WriteLine("Player {0} has won!", x)
                                                true
                                             else gameOverRows t

let gameOverCols board = false

let gameOverDiags board = false

let gameOver board = 
    let rowWin = gameOverRows board
    let colWin = gameOverCols board
    let diagWin = gameOverDiags board
    if(rowWin || colWin || diagWin) then
        true
    else 
        false

//=======================================
// input
//=======================================

let pieceValid piece = piece = "x" || piece = "o"

let readInPiece () = 
    System.Console.Write("Read in piece 'x' or 'o': ")
    let mutable piece = System.Console.ReadLine()
    while pieceValid piece = false do
        System.Console.WriteLine("Valid pieces are 'x' and 'o'. You entered {0}", piece)
        piece <- System.Console.ReadLine()
    piece

let readInPosition boardSize = 
    let getPos = fun (label : string) -> 
                        System.Console.Write("{0}: ", label)
                        let mutable item = System.Console.ReadLine()
                        
                        let validMove = fun x -> x >= 0 && x < boardSize
                        
                        while validMove (System.Convert.ToInt32 item) = false do
                            System.Console.WriteLine("Enter a valid move (0 to {0})", boardSize - 1)
                            item <- System.Console.ReadLine()
                        System.Convert.ToInt32(item)

    (getPos "row", getPos "col")
        
//===========================================
// play game
//===========================================

let rec playGame board = 
    if gameOver board then 
        System.Console.WriteLine("Game over!")
        System.Console.ReadKey()
    else
        printBoard board
        let move = (readInPiece(), readInPosition board.Length)
        match move with 
            | (token, (row, col)) -> playGame (setToken board (row, col) token)
        

ignore(playGame (createBoard 3))