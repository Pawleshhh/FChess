module FChess.Core.Tests.ChessboardTest

open NUnit.Framework
open FChess.Core.Chessboard
open FChess.Core.ChessPiece

type Column = ChessColumn
type Row = ChessRow

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
    let rows = fromRow Row.Five Direction.Bottom ()

    CollectionAssert.AreEqual([Row.One; Row.Two; Row.Three; Row.Four; Row.Five], rows)

[<Test>]
let ``fromRow, expect from specified row to top direction rows`` () =
    let rows = fromRow Row.Five Direction.Top ()

    CollectionAssert.AreEqual([Row.Five; Row.Six; Row.Seven; Row.Eight], rows)

[<Test>]
let ``fromColumn, expect from specified column to left direction columns`` () =
    let columns = fromColumn Column.E Direction.Left ()

    CollectionAssert.AreEqual([Column.A; Column.B; Column.C; Column.D; Column.E], columns)

[<Test>]
let ``fromColumn, expect from specified column to right direction columns`` () =
    let columns = fromColumn Column.E Direction.Right ()

    CollectionAssert.AreEqual([Column.E; Column.F; Column.G; Column.H], columns)

[<Test>]
let ``getRow, expect row 5 from column E to left direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow Row.Four (fromColumn Column.E Direction.Left)

    Assert.AreEqual(5, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst f.Coords = Row.Four))
    CollectionAssert.AreEqual(
        [Column.A; Column.B; Column.C; Column.D; Column.E], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getRow, expect row 5 from column E to right direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow Row.Four (fromColumn Column.E Direction.Right)

    Assert.AreEqual(4, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst(f.Coords) = Row.Four))
    CollectionAssert.AreEqual(
        [Column.E; Column.F; Column.G; Column.H], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getRow, expect row 5 from column E to column C`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow Row.Four (fromTo Column.E Column.C)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst f.Coords = Row.Four))
    CollectionAssert.AreEqual(
        [Column.C; Column.D; Column.E], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getRow, expect row 5 from column E to column G`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getRow Row.Four (fromTo Column.E Column.G)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> fst f.Coords = Row.Four))
    CollectionAssert.AreEqual(
        [Column.E; Column.F; Column.G], 
        row
        |> List.map (fun f -> snd f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to bottom direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn Column.E (fromRow Row.Five Direction.Bottom)

    Assert.AreEqual(5, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = Column.E))
    CollectionAssert.AreEqual(
        [Row.One; Row.Two; Row.Three; Row.Four; Row.Five], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to top direction`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn Column.E (fromRow Row.Five Direction.Top)

    Assert.AreEqual(4, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = Column.E))
    CollectionAssert.AreEqual(
        [Row.Five; Row.Six; Row.Seven; Row.Eight], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to row 3`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn Column.E (fromTo Row.Five Row.Three)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = Column.E))
    CollectionAssert.AreEqual(
        [Row.Three; Row.Four; Row.Five], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``getColumn, expect column E from row 5 to row 7`` () =
    let chessboard = create initialFieldInfo
    
    let row = chessboard |> getColumn Column.E (fromTo Row.Five Row.Seven)

    Assert.AreEqual(3, row.Length)
    Assert.IsTrue(row |> List.forall (fun f -> snd f.Coords = Column.E))
    CollectionAssert.AreEqual(
        [Row.Five; Row.Six; Row.Seven], 
        row
        |> List.map (fun f -> fst f.Coords))

[<Test>]
let ``diagonalFromCoords, expect coords from (4, C) into TopRight direction`` () =
    let diagonal = diagonalFromCoords (Row.Four, Column.C) DiagonalDirection.TopRight

    CollectionAssert.AreEqual(
        [(Row.Four, Column.C); (Row.Five, Column.D); (Row.Six, Column.E);
        (Row.Seven, Column.F); (Row.Eight, Column.G);],
        diagonal)

[<Test>]
let ``diagonalFromCoords, expect coords from (4, C) into BottomRight direction`` () =
    let diagonal = diagonalFromCoords (Row.Four, Column.C) DiagonalDirection.BottomRight

    CollectionAssert.AreEqual(
        [(Row.Four, Column.C); (Row.Three, Column.D); (Row.Two, Column.E);
        (Row.One, Column.F);],
        diagonal)

[<Test>]
let ``diagonalFromCoords, expect coords from (4, C) into BottomLeft direction`` () =
    let diagonal = diagonalFromCoords (Row.Four, Column.C) DiagonalDirection.BottomLeft

    CollectionAssert.AreEqual(
        [(Row.Four, Column.C); (Row.Three, Column.B); (Row.Two, Column.A);],
        diagonal)

[<Test>]
let ``diagonalFromCoords, expect coords from (4, C) into TopLeft direction`` () =
    let diagonal = diagonalFromCoords (Row.Four, Column.C) DiagonalDirection.TopLeft

    CollectionAssert.AreEqual(
        [(Row.Four, Column.C); (Row.Five, Column.B); (Row.Six, Column.A);],
        diagonal)

[<Test>] // Top Right
let ``diagonalCoordsFromTo, expect coords from (4, C) to (7, F)`` () =
    let diagonal = diagonalCoordsFromTo (Row.Four, Column.C) (Row.Seven, Column.F)

    CollectionAssert.AreEqual(
        [(Row.Four, Column.C);(Row.Five, Column.D);(Row.Six, Column.E); (Row.Seven, Column.F)],
        diagonal)

[<Test>] // Bottom Left
let ``diagonalCoordsFromTo, expect coords from (4, C) to (2, A)`` () =
    let diagonal = diagonalCoordsFromTo (Row.Four, Column.C) (Row.Two, Column.A)

    CollectionAssert.AreEqual(
        [(Row.Four, Column.C);(Row.Three, Column.B);(Row.Two, Column.A);],
        diagonal)

[<Test>] // Top Left
let ``diagonalCoordsFromTo, expect coords from (4, H) to (6, F)`` () =
    let diagonal = diagonalCoordsFromTo (Row.Four, Column.H) (Row.Six, Column.F)

    CollectionAssert.AreEqual(
        [(Row.Four, Column.H);(Row.Five, Column.G);(Row.Six, Column.F)],
        diagonal)

[<Test>] // Bottom Right
let ``diagonalCoordsFromTo, expect coords from (7, C) to (4, F)`` () =
    let diagonal = diagonalCoordsFromTo (Row.Seven, Column.C) (Row.Four, Column.F)

    CollectionAssert.AreEqual(
        [(Row.Seven, Column.C);(Row.Six, Column.D);(Row.Five, Column.E);(Row.Four, Column.F)],
        diagonal)