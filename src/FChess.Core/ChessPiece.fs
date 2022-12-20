module FChess.Core.ChessPiece

type Side =
    | White = 0
    | Black = 1

type ChessRow = 
    | One = 0
    | Two = 1 
    | Three = 2 
    | Four = 3 
    | Five = 4 
    | Six = 5 
    | Seven = 6 
    | Eight = 7

type ChessColumn =
    | A = 0
    | B = 1
    | C = 2
    | D = 3
    | E = 4
    | F = 5
    | G = 6
    | H = 7

type ChessPieceInfo = { Side : Side; Coords : ChessRow * ChessColumn }

type ChessPiece =
    | Empty
    | Pawn of Side
    | Knight of Side
    | Bishop of Side
    | Rook of Side
    | Queen of Side
    | King of Side
    member self.Value =
        match self with
        | Pawn _ -> 1
        | Knight _ -> 3
        | Bishop _ -> 3
        | Rook _ -> 5
        | Queen _ -> 9
        | _ -> 0