package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessBoard.ChessBoard
import com.almikey.chesschallenge.ChessPieces.{
  Bishop,
  Blank,
  ChessPiece,
  Knight
}
import org.scalatest.{FlatSpec, Matchers}

class ChessBoardTest extends FlatSpec with Matchers {
  "generate 2 by 2 chessboard" should "return a vector of size 4" in {
    var newBoard = ChessBoard.generateBoard(2, 2)
    assert(newBoard.isInstanceOf[Vector[((Int, Int), ChessPiece)]])
    assert(newBoard.size == 4)
  }
  "new chess boards" should "have Blank() chess pieces" in {
    var newBoard = ChessBoard.generateBoard(3, 4)
    newBoard.foreach { x =>
      assert(x._2.isInstanceOf[Blank])
    }
  }
  "placing a ChessPiece on 1 by 1 board" should "have a board with the same chessPiece and no attack positions" in {
    var oneByOneBoard = ChessBoard.generateBoard(1, 1)
    var knightKing = Knight()
    var expected =
      Right((Vector(((1, 1), Knight())), Vector.empty[(Int, Int)]))
    def alwayPassingCondition = (_: ChessPiece) => false
    var result = ChessBoard.placePieceOnBoard(
      oneByOneBoard,
      knightKing,
      (1, 1),
      Vector.empty[(Int, Int)],
      alwayPassingCondition
    )
    result shouldEqual expected
  }
  "placing a Knight in position (1,1) on 3 by 3 board" should "have a board with 2 positions under attack (2,3) and (3,2)" in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knightKing = Knight()
    var expected = Vector((2, 3), (3, 2))
    //a condition that if true stops the placing of the piece
    def alwayFailingCondition = (_: ChessPiece) => false
    var result = ChessBoard.placePieceOnBoard(
      threeByThreeBoard,
      knightKing,
      (1, 1),
      Vector.empty[(Int, Int)],
      alwayFailingCondition
    )
    result.merge._2 shouldEqual expected
  }

  "removing a Knight in position (1,1) on 3 by 3 board with no other pieces" should "have a board with no positions " +
    "under " in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knightKing = Knight()
    knightKing.position = (1, 1)
    var expectedPositionsUnderAttack = Vector((2, 3), (3, 2))
    var expected = Vector.empty[(Int, Int)]
    //a condition that if true stops the placing of the piece
    def alwayFalingCondition = (_: ChessPiece) => false
    var result = ChessBoard.placePieceOnBoard(
      threeByThreeBoard,
      knightKing,
      (1, 1),
      Vector.empty[(Int, Int)],
      alwayFalingCondition
    )
    var afterRemovalResult = ChessBoard.removePieceFromBoard(
      result.merge._1,
      knightKing,
      (1, 1),
      knightKing.attackingPositions(threeByThreeBoard).toVector
    )
    println(afterRemovalResult)
    afterRemovalResult.merge._2 shouldEqual expected
  }

  "placing a chess piece in boxes under attack or near pieces it can attack through placePieceOnBoard method " should "return false to abort action" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var bishop1 = Bishop()
    bishop1.position = (1, 1)
    var bishop2 = Bishop()
    bishop2.position = (2, 2)

    def alwayFalingCondition = (_: ChessPiece) => false

    var (newBoard, noGoSlots) = ChessBoard
      .placePieceOnBoard(
        twoByTwoBoard,
        bishop1,
        (1, 1),
        Vector.empty[(Int, Int)],
        alwayFalingCondition
      )
      .merge
    def chekingCondition =
      ChessBoard.tellMeIfNoGoMethodBuilder(noGoSlots, newBoard)
    var result = ChessBoard.placePieceOnBoard(
      newBoard,
      bishop2,
      (2, 2),
      noGoSlots,
      chekingCondition
    )
    println(result)
    result.isLeft shouldBe true
  }
  "adding a piece to a box with another non blank piece i.e. occupied by other piece" should "return an Either, Left(Chessboard) to indicate failure with chessboard remaining the same" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var bishop1 = Bishop()
    bishop1
    var bishop2 = Bishop()
    def alwayFalingCondition = (_: ChessPiece) => false
    //Left case
    var chessBoard: ChessBoard = Vector(
      ((1, 1), bishop1),
      ((1, 2), Blank()),
      ((2, 1), Blank()),
      ((2, 2), Blank())
    )

    var result = ChessBoard.placePieceOnBoard(
      chessBoard,
      bishop2,
      (1, 1),
      Vector.empty[(Int, Int)],
      alwayFalingCondition
    )
    var expected = Left(
      (
        Vector(
          ((1, 1), Bishop()),
          ((1, 2), Blank()),
          ((2, 1), Blank()),
          ((2, 2), Blank())
        ),
        Vector()
      )
    )
    result shouldEqual expected
  }
  "adding a piece to a box with a blank piece" should "return an Either, Right(Chessboard) to indicate success " in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var bishop1 = Bishop()
    var bishop2 = Bishop()
    def alwayFalingCondition = (_: ChessPiece) => false
    //Left case
    var chessBoard: ChessBoard = Vector(
      ((1, 1), bishop1),
      ((1, 2), Blank()),
      ((2, 1), Blank()),
      ((2, 2), Blank())
    )

    var result = ChessBoard.placePieceOnBoard(
      chessBoard,
      bishop2,
      (1, 2),
      Vector.empty[(Int, Int)],
      alwayFalingCondition
    )
    var expected = Right(
      (
        Vector(
          ((1, 1), Bishop()),
          ((1, 2), Bishop()),
          ((2, 1), Blank()),
          ((2, 2), Blank())
        ),
        Vector((2, 1))
      )
    )
    result shouldEqual expected
  }
  "trying to add a piece that will threaten a piece" should "return an Either, Left(Chessboard) with no change in board to indicate failure " in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var knight1 = Knight()
    var bishop2 = Bishop()
    def alwayFalingCondition = (_: ChessPiece) => false
    //Left case

    var placeKnight1 = ChessBoard.placePieceOnBoard(
      twoByTwoBoard,
      knight1,
      (1, 1),
      Vector.empty[(Int, Int)],
      alwayFalingCondition
    )
    var chessBoard2 = placeKnight1.merge
//    println(chessBoard2)
    var checkingCondition =
      ChessBoard.tellMeIfNoGoMethodBuilder(chessBoard2._2, chessBoard2._1)
    var placeBishop2 = ChessBoard.placePieceOnBoard(
      chessBoard2._1,
      bishop2,
      (2, 2),
      chessBoard2._2,
      checkingCondition
    )
//no difference by placing bishop2 action as it will fail due to threatening other piece
    placeBishop2.merge shouldEqual placeKnight1.merge
    placeBishop2.isLeft shouldBe true
    placeKnight1.isRight shouldBe true
  }

}
