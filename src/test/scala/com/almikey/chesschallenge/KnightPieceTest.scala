package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{ChessPiece, Knight}
import org.scalatest.{FlatSpec, Matchers}

class KnightPieceTest extends FlatSpec with Matchers {
  "A knight piece" should "have no positions to attack if the size of the board is less than 2 by 2" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var knight: ChessPiece = Knight((1, 1), twoByTwoBoard)
    knight.attackingPositions(twoByTwoBoard) shouldEqual Nil
  }
  it should "return true if another piece is in its attack position" in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knight1: ChessPiece = Knight((1, 1), threeByThreeBoard)
    var knight2: ChessPiece = Knight((2, 3), threeByThreeBoard)
    knight1.canCaptureOther(knight2) shouldEqual true
  }
}
