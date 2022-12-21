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

    let chessboard = create initialFieldInfo

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

    let chessboard = create initialFieldInfo

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

    let chessboard = create initialFieldInfo

    chessboard[(if side = Side.White then 1 else 6), *]
    |> Array.iteri (fun c f -> 
        Assert.That(f.ChessPiece, Is.EqualTo expectedFirstRowPieces[c]))

[<Test>]
let ``create initial chessboard, expect empty fields`` () =
    let chessboard = create initialFieldInfo
    
    chessboard[2..5, *]
    |> Array2D.iter (fun f -> Assert.IsTrue(match f.ChessPiece with | ChessPiece.Empty -> true | _ -> false))

[<Test>]
let ``fromRow, expect from specified row to bottom direction rows`` () =
    let rows = fromRow ChessRow.Five Direction.Bottom ()

    CollectionAssert.AreEqual([ChessRow.One; ChessRow.Two; ChessRow.Three; ChessRow.Four; ChessRow.Five], rows)

[<Test>]
let ``fromRow, expect from specified row to top direction rows`` () =
    let rows = fromRow ChessRow.Five Direction.Top ()

    CollectionAssert.AreEqual([ChessRow.Five; ChessRow.Six; ChessRow.Seven; ChessRow.Eight], rows)

[<Test>]
let ``fromColumn, expect from specified column to left direction columns`` () =
    let columns = fromColumn ChessColumn.E Direction.Left ()

    CollectionAssert.AreEqual([ChessColumn.A; ChessColumn.B; ChessColumn.C; ChessColumn.D; ChessColumn.E], columns)

[<Test>]
let ``fromColumn, expect from specified column to right direction columns`` () =
    let columns = fromColumn ChessColumn.E Direction.Right ()

    CollectionAssert.AreEqual([ChessColumn.E; ChessColumn.F; ChessColumn.G; ChessColumn.H], columns)

[<Test>]
let ``getRow, expect row 5 from column E to left direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow ChessRow.Four (fromColumn ChessColumn.E Direction.Left)

    Assert.AreEqual(5, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst f.Coords = ChessRow.Four))
    CollectionAssert.AreEqual(
        [ChessColumn.A; ChessColumn.B; ChessColumn.C; ChessColumn.D; ChessColumn.E], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getRow, expect row 5 from column E to right direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow ChessRow.Four (fromColumn ChessColumn.E Direction.Right)

    Assert.AreEqual(4, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst(f.Coords) = ChessRow.Four))
    CollectionAssert.AreEqual(
        [ChessColumn.E; ChessColumn.F; ChessColumn.G; ChessColumn.H], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getRow, expect row 5 from column E to column C`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow ChessRow.Four (fromTo<ChessColumn> 4 2)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst f.Coords = ChessRow.Four))
    CollectionAssert.AreEqual(
        [ChessColumn.C; ChessColumn.D; ChessColumn.E], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getRow, expect row 5 from column E to column G`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow ChessRow.Four (fromTo<ChessColumn> 4 6)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst f.Coords = ChessRow.Four))
    CollectionAssert.AreEqual(
        [ChessColumn.E; ChessColumn.F; ChessColumn.G], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to bottom direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn ChessColumn.E (fromRow ChessRow.Five Direction.Bottom)

    Assert.AreEqual(5, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = ChessColumn.E))
    CollectionAssert.AreEqual(
        [ChessRow.One; ChessRow.Two; ChessRow.Three; ChessRow.Four; ChessRow.Five], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to top direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn ChessColumn.E (fromRow ChessRow.Five Direction.Top)

    Assert.AreEqual(4, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = ChessColumn.E))
    CollectionAssert.AreEqual(
        [ChessRow.Five; ChessRow.Six; ChessRow.Seven; ChessRow.Eight], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to row 3`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn ChessColumn.E (fromTo<ChessRow> 4 2)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = ChessColumn.E))
    CollectionAssert.AreEqual(
        [ChessRow.Three; ChessRow.Four; ChessRow.Five], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to row 7`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn ChessColumn.E (fromTo<ChessRow> 4 6)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = ChessColumn.E))
    CollectionAssert.AreEqual(
        [ChessRow.Five; ChessRow.Six; ChessRow.Seven], 
        row
        |> List.map (fun f -> fst f.Coords))