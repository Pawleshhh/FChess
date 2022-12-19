module FChess.Core.ChessPiece

type Side =
    | White
    | Black

type ChessRow = 
    | One = 1 
    | Two = 2 
    | Three = 3 
    | Four = 4 
    | Five = 5 
    | Six = 6 
    | Seven = 7 
    | Eight = 8

type ChessColumn =
    | A = 1
    | B = 2
    | C = 3
    | D = 4
    | E = 5
    | F = 6
    | G = 7
    | H = 8

type ChessPieceInfo = { Side : Side; Coords : ChessRow * ChessColumn }

type ChessPiece =
    | None of Side
    | Pawn of Side
    | Knight of Side
    | Bishop of Side
    | Rook of Side
    | Queen of Side
    | King of Side