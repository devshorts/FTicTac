module Board

exception DimensionException of string

open TicTacToeData
open Player

let private reverseList list = List.fold (fun acc elem -> elem::acc) [] list

//---------------------------------
// create the board
//---------------------------------

let createBoard dimension =     
    let rec createRow width = 
        if width = 0 then []
        else CellElement.None::createRow(width - 1)

    let rec createBoardHelper dimension init = 
        if(init = 0) then []
        else createRow(dimension)::(createBoardHelper dimension (init-1))

    createBoardHelper dimension dimension

//---------------------------------
// get from board
//---------------------------------

let rec private getRowElement row element = 
    if element = 0 then
        match row with
            | [] -> raise(DimensionException("Didn't find a row"))
            | h::_ -> h
    else 
        match row with
            | [] -> raise(DimensionException("Didn't find a row"))
            | h::t -> getRowElement t (element-1)

let private getPosition board (row,col) = 
    let rowArray = getRowElement board row
    getRowElement rowArray col

//---------------------------------
// print board
//---------------------------------

let rec private printRow row = 
    match row with 
        | [] -> System.Console.WriteLine()
        | h::t -> 
            match h with 
               | CellElement.Some(token) -> System.Console.Write(tokenType token) 
               | CellElement.None -> System.Console.Write(" ")
            System.Console.Write(" | ")
            printRow t

let private printBoard board =
    let rec printBoardHelper board = 
        match board with 
            | [] -> System.Console.WriteLine()
            | h::[] -> printRow h
            | h::t -> 
                printRow h
                printBoardHelper t

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
let private updateToken targetItem doUpdate currentItem = 
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

let private applyMoveToRow currentRow targetColumnIndex token = 
    // if we're on the current row, we apply a map function to the row
    // to transform it, but we only want to change the actual element at the
    // column index

    let startColumnIndex = ref 0
    let isCorrectColumnFunc = fun () -> 
                                    let currentColumnIndex = !startColumnIndex
                                    startColumnIndex := !startColumnIndex + 1
                                    if currentColumnIndex = targetColumnIndex then true
                                    else false

    let updateTokenFunc = updateToken (CellElement.Some(token)) isCorrectColumnFunc

    List.map updateTokenFunc currentRow

let private setToken board (row,col) token = 

    // helper that finds the current row and applies the map function to it
    let rec setTokenHelper board (row, col) token currentRowIndex = 
        
        if currentRowIndex < 0 || currentRowIndex >= List.length board then []
        else 
            let currentRow = getRowElement board currentRowIndex

            // returns the next row in the board
            let nextRow () = setTokenHelper board (row, col) token (currentRowIndex-1)

            if currentRowIndex = row then
                let updatedRow = (applyMoveToRow currentRow col token)
                updatedRow::nextRow()      
                  
            else currentRow::nextRow()

    // since the board is rebuilt backwards we need to reverse it 
    // for easy printing
    reverseList (setTokenHelper board (row,col) token (board.Length-1))


//=======================================
// detect game over
//=======================================

(*
Function to test if a row has all the same element. CountBy aggregates the list into 
(item, count) so we can fold the list into a list of tuples that tells us 
how many of the cell elements existed.  We can test each type of winner
*)
let private allCellsHaveSamePlayer cells = 
    let rowsGroupedByToken = cells |> Seq.countBy id |> Seq.toList

    let rec rowHasSameElement rowGroup = 
        match rowGroup with 
            | [] -> (false, CellElement.None)
            | h::t -> match h with 
                        | (key, count) -> if count = Seq.length cells then (true, key)
                                          else rowHasSameElement t

    rowHasSameElement rowsGroupedByToken
   
(*
Helper function to test a group of elements. All elements have to be of the same type
or they don't win.  We can pass in a recursion function so that we basically get the next 
set of elements to test.  
*)
let private isWin cellList winTypeString =
    let win = allCellsHaveSamePlayer cellList
    match win with 
        | (won, CellElement.Some(token)) -> if won then 
                                                System.Console.WriteLine("Player {0} has won a {1}!", tokenType token, winTypeString)
                                                true
                                            else false
        | (_) -> false

(*
The easiest case with just testing each row
*)
let rec private gameOverRows board = 
    match board with
        | [] -> false
        | row::nextRows -> if isWin row "row" then true
                                 else gameOverRows nextRows
                                

(* 
Check the columns. For each row in the board we want to get the same element
and create a list out of that. That is the folded column element. We can then treat
this as a generic "row" of elements and pass it to our test win function
*)
let rec private gameOverCols board columnIndex = 
    if columnIndex >= List.length board then 
        false
    else
        let columnElements = List.fold(fun columns row -> (getRowElement row columnIndex)::columns) [] board
        let nextColumn = columnIndex + 1
        if isWin columnElements "column" then true
        else gameOverCols board nextColumn
    

(*
We fold the board by selecting each element and updating our accumulator index
This lets us get item 0, then item 1, then item 2 and leveage getting an element in a row/
We store the diagonal elements via the aggregator
By psasing in the "updater" function we can either incremenet or decrement where we start from
*)
let private gameOverDiags board start updater = 
    let diagonalGroup = List.fold(fun acc row -> 
                let columnIndex = snd acc
                let nextColumnIndex = updater columnIndex
                let diagonalElements = fst acc
                (getRowElement row columnIndex)::diagonalElements, nextColumnIndex) ([],start) board

    let diagonalElements = match diagonalGroup with
                             | (list, item) -> list     
                                              
    isWin diagonalElements "diagonal"

(*
 Lazily check all the combinatoins: row, columns, diagonals
*)
let private gameOver board = 
    let rowWin = gameOverRows board
    let colWin = gameOverCols board 0
    let diagWinLeft = gameOverDiags board 0 (fun x -> x + 1)
    let diagWinRight = gameOverDiags board (board.Length-1) (fun x -> x - 1)
    if(rowWin || colWin || diagWinLeft || diagWinRight) then
        true
    else 
        false

        
//===========================================
// play game
//===========================================

// makes the players turn and returns the new board
let private takePlayerTurn board (currentPlayer:IPlayer) =     
    if gameOver board then 
        (true, board)
    else
        printBoard board
        let move = currentPlayer.play board
        match move with 
            | (token, (row, col)) -> (false, setToken board (row, col) token)
            
                                         
let rec playGame board (players:IPlayer list) =
    // run the accumulator against  (gameWinBool, newBoard) tuple
    let winBoardPair = List.fold(fun winBoardTuple currentPlayer -> 
                                        if fst winBoardTuple = true then winBoardTuple
                                        else takePlayerTurn (snd winBoardTuple) currentPlayer) (false, board) players
    let newBoard = snd winBoardPair
    let gameOver = fst winBoardPair
    if gameOver then 
        System.Console.WriteLine("Game over!")
        printBoard newBoard
        System.Console.ReadKey()
    else playGame newBoard players