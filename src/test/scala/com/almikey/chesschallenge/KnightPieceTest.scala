package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{ChessPiece, Knight}
import org.scalatest.{FlatSpec, Matchers}

class KnightPieceTest extends FlatSpec with Matchers {
  "A knight piece" should "have no positions to attack if the size of the board is less than 2 by 2" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var knight: ChessPiece = Knight()
    knight.position = (1, 1)
    knight.attackingPositions(twoByTwoBoard) shouldEqual Nil
  }
  it should "have its capture method return true if another piece is in its attack position" in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knight1: ChessPiece = Knight()
    knight1.position = (1, 1)
    var knight2: ChessPiece = Knight()
    knight2.position = (2, 3)
    knight1.canCaptureOther(knight2, threeByThreeBoard) shouldEqual true
  }
}
