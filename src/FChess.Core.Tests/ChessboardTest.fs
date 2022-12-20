module FChess.Core.Tests.ChessboardTest

open NUnit.Framework
open FChess.Core.Chessboard
open FChess.Core.ChessPiece

[<Test>]
let ``create initial chessboard, expect fields' color`` () =
    
    let getStart row =
        if row % 2 = 0 then Side.White else Side.Black

    let getSide column start =
        if column % 2 = 0 then 
            start 
        else 
            (if start = Side.White then Side.Black else Side.White)

    let expectedArray = Array2D.init 8 8 (fun i j -> getStart i |> getSide j)

    let chessboard = create ()

    chessboard
    |> Array2D.iteri (fun r c f -> Assert.That(f.Side, Is.EqualTo expectedArray[r, c] , $"At {r},{c}"))

[<TestCase(Side.White)>]
[<TestCase(Side.Black)>]
let ``create initial chessboard, expect first row side pieces on their position`` side =
    let firstRowPieces = 
        [ChessPiece.Rook;
        ChessPiece.Knight;
        ChessPiece.Bishop;
        ChessPiece.Queen;
        ChessPiece.King;
        ChessPiece.Bishop; 
        ChessPiece.Knight;
        ChessPiece.Rook]

    let expectedFirstRowPieces = firstRowPieces |> List.map (fun f -> f side)

    let chessboard = create ()

    chessboard[(if side = Side.White then 0 else 7), *]
    |> Array.iteri (fun c f -> 
        Assert.That(f.ChessPiece, Is.EqualTo expectedFirstRowPieces[c]))
        
[<TestCase(Side.White)>]
[<TestCase(Side.Black)>]
let ``create initial chessboard, expect side pawns on their position`` side =
    let expectedFirstRowPieces = 
        ChessPiece.Pawn 
        |> List.replicate 8 
        |> List.map (fun f -> f side)

    let chessboard = create ()

    chessboard[(if side = Side.White then 1 else 6), *]
    |> Array.iteri (fun c f -> 
        Assert.That(f.ChessPiece, Is.EqualTo expectedFirstRowPieces[c]))

[<Test>]
let ``create initial chessboard, expect empty fields`` () =
    let chessboard = create ()
    
    chessboard[2..5, *]
    |> Array2D.iter (fun f -> Assert.IsTrue(match f.ChessPiece with | ChessPiece.Empty -> true | _ -> false))

[<Test>]
let ``fromRow, expect from to bottom direction rows`` () =
    let rows = fromRow 4 Direction.Bottom ()

    CollectionAssert.AreEqual([ChessRow.One; ChessRow.Two; ChessRow.Three; ChessRow.Four; ChessRow.Five], rows)

[<Test>]
let ``fromRow, expect from to top direction rows`` () =
    let rows = fromRow 4 Direction.Top ()

    CollectionAssert.AreEqual([ChessRow.Five; ChessRow.Six; ChessRow.Seven; ChessRow.Eight], rows)