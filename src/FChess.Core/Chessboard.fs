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

let private enumToList<'a> = (Enum.GetValues(typeof<'a>) :?> ('a [])) |> Array.toList

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
        Coords = (enum<ChessRow>(row), enum<ChessColumn>(column)); 
        Side = side;
        ChessPiece = pieceSide row |> piece
    }

let create () =
    let chessboard = Array2D.init 8 8 (fun i j -> initialFieldInfo (i, j))
    chessboard

let getRow row fromColumn chessboard =
    fromColumn ()
    |> List.map (fun column -> chessboard |> fieldAt (row, column))
    
let fromColumn column direction =
    match direction with
    | Direction.Left -> (fun () -> enumToList<ChessColumn>[..int column])
    | Direction.Right -> (fun () -> enumToList<ChessColumn>[int column..])
    | _ -> failwith "Expected either Left or Right direction only"
    
let getColumn column fromRow chessboard =
    fromRow ()
    |> List.map (fun row -> chessboard |> fieldAt (row, column))

let fromRow row direction =
    match direction with
    | Direction.Bottom -> (fun () -> enumToList<ChessRow>[..int row])
    | Direction.Top -> (fun () -> enumToList<ChessRow>[int row..])
    | _ -> failwith "Expected either Bottom or Top direction only"

let fromTo<'a> from to' =
    match (from, to') with
        | f, t when int f > int t -> (fun () -> enumToList<'a>[int to' .. int from])
        | _ -> (fun () -> enumToList<'a>[int from .. int to'])