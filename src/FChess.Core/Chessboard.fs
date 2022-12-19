module FChess.Core.Chessboard

open System
open System.Collections.Generic
open FChess.Core.ChessPiece

type T = Dictionary<(ChessRow * ChessColumn), ChessPiece>

let private enumToList<'a> = (Enum.GetValues(typeof<'a>) :?> ('a [])) |> Array.toList

let fieldSide coords =
    let (row, column) = coords
    let isLight = (int(row) % 2) = (int(column) % 2)

    match isLight with
    | true -> Side.White
    | _ -> Side.Black

let create () =

    let rows = enumToList<ChessRow>
    let columns = enumToList<ChessColumn>
    let chessboard = new T(64)

    let getInitialPiece coords =
        let (row, column) = coords
        let side = fieldSide coords

        if List.contains row [1;2;7;8] then
            
            
            ChessPiece.None Side.Black
        else
            ChessPiece.None side


    for r in rows do
        for c in columns do
            chessboard.Add((r, c), (int(r), int(c)) |> getInitialPiece)