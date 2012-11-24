exception DimensionException of string
exception InvalidPlayer of string

type Player = 
    | X
    | O

type CellElement =
    | Some of Player
    | None

let reverseList list = List.fold (fun acc elem -> elem::acc) [] list

//---------------------------------
// player information
//---------------------------------

let mapPieceToPlayer piece = 
    match piece with 
        | "x" -> Player.X
        | "o" -> Player.O
        | _ -> raise(InvalidPlayer("Piece is invalid"))

let tokenType player = 
    match player with 
        | Player.X -> "x"
        | Player.O -> "o"

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
               | CellElement.Some(token) -> System.Console.Write(tokenType token) 
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

(*
Update token helper. Takes the target token to change to, the current token we are on
(since it's used in a folding curry'd function), and tests if its a valid move or not.
If it's not valid, it transform the current item to itself, if it is a vlaid move
it will transform the current item into the target item
*)
let updateToken targetItem doUpdate currentItem = 
    if doUpdate() then 
        match currentItem with
            | CellElement.Some(i) -> System.Console.WriteLine("move invalid")
                                     currentItem
            | CellElement.None -> targetItem

    else currentItem

(*
We need to recursively rebuild the board after setting a token. This helper does that
The iter option  looks for the target row. When it finds ht etarget row it applies a map 
function to the row to transform its elements. The map function finds the right element and
transforms it into the new token (if valid) and returns a new row with the elements updated
Then we can finish rebuilding the remainder of the board
*)
let rec setTokenHelper board (row, col) token currentRow = 
    // returns the next row in the board
    let nextRow () = setTokenHelper board (row, col) token (currentRow-1)

    if currentRow = row then
        
        // if we're on the current row, we apply a map function to the row
        // to transform it, but we only want to change the actual element at the
        // column index

        let counter = fun x -> x + 1
        let add = ref 0
        let isCorrectColumnFunc = fun () -> 
                                        let orig = !add
                                        add := counter !add 
                                        if orig = col then true
                                        else false

        let updateTokenFunc = updateToken (CellElement.Some(token)) isCorrectColumnFunc

        let rowElement = getRowElement board currentRow

        (List.map updateTokenFunc rowElement)::nextRow()
    else
        if currentRow = -1 then []            
        else
            let rowElement = getRowElement board currentRow
            rowElement::nextRow()

let setToken board (row,col) token = reverseList (setTokenHelper board (row,col) token (board.Length-1))

//=======================================
// detect game over
//=======================================

(*
Function to test if a row has all the same element. CountBy aggregates the list into 
(item, count) so we can fold the list into a list of tuples that tells us 
how many of the cell elements existed.  We can test each type of winner
*)
let rowWin row = 
    let rowsGroupedByToken = row |> Seq.countBy id |> Seq.toList

    let rec rowHasSameElement rowGroup = 
        match rowGroup with 
            | [] -> (false, CellElement.None)
            | h::t -> match h with 
                        | (key, count) -> if count = Seq.length row then (true, key)
                                          else rowHasSameElement t

    rowHasSameElement rowsGroupedByToken
   
(*
Helper function to test a group of elements. All elements have to be of the same type
or they don't win.  We can pass in a recursion function so that we basically get the next 
set of elements to test.  
*)
let testWin cellList callback winTypeString =
    let win = rowWin cellList
    match win with 
        | (won, CellElement.Some(token)) -> if won then 
                                                System.Console.WriteLine("Player {0} has won a {1}!", tokenType token, winTypeString)
                                                true
                                            else callback()
        | (_) -> callback() 

(*
The easiest case with just testing each row
*)
let rec gameOverRows board = 
    match board with
        | [] -> false
        | h::t -> testWin h (fun () -> gameOverRows t) "row"

(* 
Check the columns. For each row in the board we want to get the same element
and create a list out of that. That is the folded column element. We can then treat
this as a generic "row" of elements and pass it to our test win function
*)
let rec gameOverCols board columnIndex = 
    if columnIndex >= List.length board then 
        false
    else
        let columnElements = List.fold(fun columns row -> (getRowElement row columnIndex)::columns) [] board
        let nextColumn = columnIndex + 1
        testWin columnElements (fun () -> gameOverCols board nextColumn) "column"
    

(*
We fold the board by selecting each element and updating our accumulator index
This lets us get item 0, then item 1, then item 2 and leveage getting an element in a row/
We store the diagonal elements via the aggregator
By psasing in the "updater" function we can either incremenet or decrement where we start from
*)
let gameOverDiags board start updater = 
    let diagonalGroup = List.fold(fun acc row -> 
                let columnIndex = snd acc
                let nextColumnIndex = updater columnIndex
                let diagonalElements = fst acc
                (getRowElement row columnIndex)::diagonalElements, nextColumnIndex) ([],start) board

    let diagonalElements = match diagonalGroup with
                             | (list, item) -> list     
                                              
    testWin diagonalElements (fun () -> false) "diagonal"

(*
 Lazily check all the combinatoins: row, columns, diagonals
*)
let gameOver board = 
    let rowWin = gameOverRows board
    let colWin = gameOverCols board 0
    let diagWinLeft = gameOverDiags board 0 (fun x -> x + 1)
    let diagWinRight = gameOverDiags board (board.Length-1) (fun x -> x - 1)
    if(rowWin || colWin || diagWinLeft || diagWinRight) then
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
    done

    mapPieceToPlayer piece

let readInPosition boardSize = 
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
        
//===========================================
// play game
//===========================================

let rec playGame board humanMoveFunc = 
    if gameOver board then 
        System.Console.WriteLine("Game over!")
        printBoard board
        System.Console.ReadKey()
    else
        printBoard board
        let move = humanMoveFunc board
        match move with 
            | (token, (row, col)) -> 
                let newBoard = setToken board (row, col) token
                playGame newBoard humanMoveFunc

//=======================================
// human player logic
//=======================================
        
(*
returns a tuple of player token with a tuple of (row, col)
*)

let humanPlayerMove board = (readInPiece(), readInPosition (List.length board))

ignore(playGame (createBoard 3) humanPlayerMove)