module FChess.Core.Chessboard

open System
open FChess.Core.ChessPiece

type FieldInfo = { Coords : ChessRow * ChessColumn; Side : Side; ChessPiece : ChessPiece }
type T = FieldInfo[,]

type Direction =
    | Top = 1
    | Right = 2
    | Bottom = 3
    | Left = 4

type DiagonalDirection =
    | TopRight = 1
    | BottomRight = 2
    | BottomLeft = 3
    | TopLeft = 4

let oppositeDirection direction =
    match direction with
    | Direction.Top -> Direction.Bottom
    | Direction.Bottom -> Direction.Top
    | Direction.Left -> Direction.Right
    | Direction.Right -> Direction.Left
    | _ -> failwith "Unexpected direction value"

let private enumToList<'a when 'a :> Enum> = (Enum.GetValues(typeof<'a>) :?> ('a [])) |> Array.toList
let private eint (v : 'a) = Convert.ToInt32 v

let fieldAt (coords : ChessRow * ChessColumn) (chessboard : T) =
    let (row, column) = coords
    chessboard[int row, int column]

let lastColumn (direction : Direction) =
    match direction with
    | Direction.Left -> Some ChessColumn.A
    | Direction.Right -> Some ChessColumn.H
    | _ -> None

let fieldSide coords =
    let (row, column) = coords
    let isLight = ((row + 1) % 2) = ((column + 1) % 2)

    match isLight with
    | true -> Side.White
    | _ -> Side.Black

let pieceSide row =
    match row with
    | row when row <= 1 -> Side.White
    | _ -> Side.Black

let initialFieldInfo coords =
    let (row, column) = coords
    let side = fieldSide coords

    let firstRowPieces = 
        [ChessPiece.Rook;
        ChessPiece.Knight;
        ChessPiece.Bishop;
        ChessPiece.Queen;
        ChessPiece.King;
        ChessPiece.Bishop; 
        ChessPiece.Knight;
        ChessPiece.Rook]

    let piece = 
        match (row, column) with
        | (r, _) when List.contains r [1; 6] -> ChessPiece.Pawn
        | (r, c) when List.contains r [0; 7] -> firstRowPieces.[c]
        | _ -> fun _ -> ChessPiece.Empty

    { 
        Coords = (enum<ChessRow> row, enum<ChessColumn> column); 
        Side = side;
        ChessPiece = pieceSide row |> piece
    }

let emptyFieldInfo coords =
    let side = fieldSide coords
    let row, column = coords
    { 
        Coords = (enum<ChessRow> row, enum<ChessColumn> column); 
        Side = side;
        ChessPiece = ChessPiece.Empty
    }

let create initialField =
    let chessboard = Array2D.init 8 8 (fun i j -> initialField (i, j))
    chessboard

let getRow row fromColumn chessboard =
    fromColumn ()
    |> List.map (fun column -> chessboard |> fieldAt (row, column))
    
let fromColumn (column : ChessColumn) direction =
    match direction with
    | Direction.Left -> (fun () -> enumToList<ChessColumn>[..int column])
    | Direction.Right -> (fun () -> enumToList<ChessColumn>[int column..])
    | _ -> failwith "Expected either Left or Right direction only"
    
let getColumn column fromRow chessboard =
    fromRow ()
    |> List.map (fun row -> chessboard |> fieldAt (row, column))

let fromRow (row : ChessRow) direction =
    match direction with
    | Direction.Bottom -> (fun () -> enumToList<ChessRow>[..int row])
    | Direction.Top -> (fun () -> enumToList<ChessRow>[int row..])
    | _ -> failwith "Expected either Bottom or Top direction only"

let fromTo<'a when 'a :> Enum> (from : 'a) (to' : 'a) =
    match (from, to') with
        | f, t when eint f > eint t -> (fun () -> enumToList<'a>[eint to' .. eint from])
        | _ -> (fun () -> enumToList<'a>[eint from .. eint to'])

let getDiagonal from chessboard =
    from ()
    |> List.map (fun (row, column) -> chessboard |> fieldAt (row, column))

let diagonalFromCoords (coords : (ChessRow * ChessColumn)) direction =
    let (row, column) = coords
    let (rowSign, columnSign) = 
        match direction with
        | DiagonalDirection.TopRight -> (1, 1)
        | DiagonalDirection.BottomRight -> (-1, 1)
        | DiagonalDirection.BottomLeft -> (-1, -1)
        | DiagonalDirection.TopLeft -> (1, -1)
        | _ -> failwith "Unexpected diaglonal direction"

    let rec iterate i j result =
        if i < 0 || i > 7 || j < 0 || j > 7 then
            result
        else
            iterate (i + rowSign) (j + columnSign) (result @ [(enum<ChessRow> i, enum<ChessColumn> j)])

    fun () -> iterate (int row) (int column) List.empty<(ChessRow * ChessColumn)>

let diagonalCoordsFromTo (from : (ChessRow * ChessColumn)) (to' : (ChessRow * ChessColumn)) =
    let (rowSign, columnSign, endRowIndex, endColumnIndex) = 
        match from, to' with
        | (sr, sc), (er, ec) when abs (eint sr - eint er) <> abs (eint sc - eint ec) -> failwith "Given coords cannot make diagonal"
        | (sr, sc), (er, ec) when sr < er && sc < ec -> (1, 1, eint er, eint ec)
        | (sr, sc), (er, ec) when sr > er && sc < ec -> (-1, 1, eint er, eint ec)
        | (sr, sc), (er, ec) when sr > er && sc > ec -> (-1, -1, eint er, eint ec)
        | (sr, sc), (er, ec) when sr < er && sc > ec -> (1, -1, eint er, eint ec)
        | _ -> failwith "Unexpected diaglonal direction"

    let rec iterate i j result =
        if i = (endRowIndex + rowSign) || j = (endColumnIndex + columnSign) then
            result
        else
            iterate (i + rowSign) (j + columnSign) (result @ [(enum<ChessRow> i, enum<ChessColumn> j)])

    let (startRow, startColumn) = from
    fun () -> iterate (int startRow) (int startColumn) List.empty<(ChessRow * ChessColumn)>