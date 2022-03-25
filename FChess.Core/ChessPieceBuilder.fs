namespace FChess.Core

open System.Collections.Generic
open ChessInfo

module ChessPieceBuilder = 

    type ChessPieceType = Pawn | Knight | Bishop | Rook | Queen | King
    
    let GetMoveRule chessPieceType =
        fun chessboard -> [(0, 0)]

    let BuildChessPiece (x, y) side chessPieceType =
        let moveRule = GetMoveRule chessPieceType
        match chessPieceType with
        | Pawn -> ChessPiece.Pawn({ x = x; y = y; side = side; moves = moveRule })
    
    //let DefaultChessPiece side =
        