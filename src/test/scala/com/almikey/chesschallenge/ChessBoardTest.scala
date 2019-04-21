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
    var knightKing = Knight((1, 1))
    var expected =
      Right((Vector(((1, 1), Knight(1, 1))), Vector.empty[(Int, Int)]))
    def alwayPassingCondition = (_: ChessPiece) => false
    var result = ChessBoard.placePieceOnBoard(
      oneByOneBoard,
      knightKing,
      (1, 1),
      Vector.empty[(Int, Int)],
      alwayPassingCondition
    )
    expected shouldEqual result
  }
  "placing a Knight in position (1,1) on 3 by 3 board" should "have a board with 2 positions under attack (2,3) and (3,2)" in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knightKing = Knight((1, 1))
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
    expected shouldEqual result.merge._2
  }

  "removing a Knight in position (1,1) on 3 by 3 board with no other pieces" should "have a board with no positions under " in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knightKing = Knight((1, 1))
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
      threeByThreeBoard,
      knightKing,
      (1, 1),
      knightKing.attackingPositions(threeByThreeBoard).toVector
    )
    afterRemovalResult.merge._2 shouldEqual expected
  }

  "placing a chess piece in boxes under attack or near pieces it can attack " should "return false to abort action" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var bishop1 = Bishop(1, 1)
    var bishop2 = Bishop(1, 2)
    def alwayFalingCondition = (_: ChessPiece) => false
    var chessBoard: ChessBoard = Vector(
      ((1, 1), bishop1),
      ((1, 2), Blank(1, 2)),
      ((2, 1), Blank(2, 1)),
      ((2, 2), Blank(2, 2))
    )
//takes a list of spots we shouldn't place a piece and creates
    //a function that returns false when you try to place a piece there
    def dontGoToBox1 =
      ChessBoard.tellMeIfNoGoMethodBuilder(Vector((1, 1)), chessBoard)
    println(chessBoard)
//    dontGoToBox1(bishop1) shouldEqual false
    dontGoToBox1(bishop2) shouldEqual false
  }

}
