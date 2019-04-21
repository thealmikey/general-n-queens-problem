package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Blank, ChessPiece, Knight}
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
    println(result)
    expected shouldEqual result
  }
}
