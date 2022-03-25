namespace FChess.Core

open System.Collections.Generic

module ChessInfo =

    type ChessSide = White | Black

    type ChessPieceInfo = {
        x : int
        y : int
        side : ChessSide
        moves : ChessboardInfo -> (int * int) list
    }
    
    and ChessPiece =
    | Pawn of ChessPieceInfo
    | Knight of ChessPieceInfo
    | Bishop of ChessPieceInfo
    | Rook of ChessPieceInfo
    | Queen of ChessPieceInfo
    | King of ChessPieceInfo
    
    and ChessboardInfo = {
        length : int
        width : int
        chessPieces : HashSet<ChessPiece>
    }