package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{
  Bishop,
  ChessPiece,
  King,
  Knight,
  Rook
}
import org.scalatest.{FlatSpec, Matchers}

class KingPieceTest extends FlatSpec with Matchers {
  "A king piece" should "attack 3 spaces in a 2 by 2 board" in {
    var threeByThreeBoard = ChessBoard.generateBoard(2, 2)
    var king: ChessPiece = King(1, 1)
    king.attackingPositions(threeByThreeBoard).size shouldEqual 3
  }
  it should "have its capture method return true when another piece is in a position it can attack" in {
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    var knight1: ChessPiece = Knight((2, 1))
    var bishop1: ChessPiece = Bishop((1, 2))
    var bishop2: ChessPiece = Bishop((2, 2))
    var bishop3: ChessPiece = Bishop((2, 3))
    var king: ChessPiece = King((1, 1))
    king.canCaptureOther(knight1, threeByThreeBoard) shouldEqual true
    king.canCaptureOther(bishop1, threeByThreeBoard) shouldEqual true
    king.canCaptureOther(bishop2, threeByThreeBoard) shouldEqual true
    king.canCaptureOther(bishop3, threeByThreeBoard) shouldEqual false
  }
}
