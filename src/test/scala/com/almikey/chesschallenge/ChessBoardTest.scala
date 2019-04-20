package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Blank, ChessPiece}
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
}
