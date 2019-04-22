package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Bishop, ChessPiece, Knight, Rook}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.Assertions._

class RookPieceTest extends FlatSpec with Matchers {
  "A Rook piece" should "should have no positions to attack if the board is 1 by 1" in {
    var oneByOneBoard = ChessBoard.generateBoard(1, 1)
    var rook: ChessPiece = Rook()
    rook.attackingPositions(oneByOneBoard) shouldEqual Nil
  }
  it should "attack 2 spaces in a 2 by 2 board" in {
    var threeByThreeBoard = ChessBoard.generateBoard(2, 2)
    var rook: ChessPiece = Rook()
    rook.position = (1, 1)
    rook.attackingPositions(threeByThreeBoard).size shouldEqual 2
  }
  it should "have its capture method return true when another piece is in a position it can attack" in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knight1: ChessPiece = Knight()
    knight1.position = (2, 1)
    var bishop1: ChessPiece = Bishop()
    bishop1.position = (1, 2)
    var rook: ChessPiece = Rook()
    rook.position = (1, 1)
    rook.canCaptureOther(knight1, threeByThreeBoard) shouldEqual true
    rook.canCaptureOther(bishop1, threeByThreeBoard) shouldEqual true
  }
}
