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

(*
Function to test if a row has all the same element. CountBy aggregates the list into 
(item, count) so we can fold the list into a list of tuples that tells us 
how many of the cell elements existed.  We can test each type of winner
*)
let rowWin row = 
    let sequences = row |> Seq.countBy id |> Seq.toList

    let rec testSequences seq = 
        match seq with 
            | [] -> (false, CellElement.None)
            | h::t -> match h with 
                        | (key, count) -> if count = Seq.length row then (true, key)
                                          else testSequences t

    testSequences sequences
   
(*
Helper function to test a group of elements. All elements have to be of the same type
or they don't win.  We can pass in a recursion function so that we basically get the next 
set of elements to test.  
*)
let testWin elements recursion wintype =
    let win = rowWin elements
    match win with 
        | (won, CellElement.Some(n)) -> if won then 
                                                System.Console.WriteLine("Player {0} has won a {1}!", n, wintype)
                                                true
                                            else recursion()
        | (_) -> recursion() 

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
let rec gameOverCols board start = 
    if start >= List.length board then 
        false
    else
        let elements = List.fold(fun columns row -> (getRowElement row start)::columns) [] board
        testWin elements (fun () -> gameOverCols board (start+1)) "column"
    

(*
We fold the board by selecting each element and updating our accumulator index
This lets us get item 0, then item 1, then item 2 and leveage getting an element in a row
By psasing in the "updater" function we can either incremenet or decrement where we start from
*)
let gameOverDiags board start updater = 
    let elements = List.fold(fun acc row -> 
                let index = snd acc
                let src = fst acc
                (getRowElement row index)::src, updater index) ([],start) board

    let item = match elements with
                 | (list, item) -> list     
                                              
    testWin item (fun () -> false) "diagonal"

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
        printBoard board
        System.Console.ReadKey()
    else
        printBoard board
        let move = (readInPiece(), readInPosition board.Length)
        match move with 
            | (token, (row, col)) -> playGame (setToken board (row, col) token)
        

ignore(playGame (createBoard 3))