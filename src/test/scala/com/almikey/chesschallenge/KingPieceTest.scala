package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{ChessPiece, King}
import org.scalatest.{FlatSpec, Matchers}

class KingPieceTest extends FlatSpec with Matchers {
  it should "attack 3 spaces in a 2 by 2 board" in {
    var threeByThreeBoard = ChessBoard.generateBoard(2, 2)
    var king: ChessPiece = King(1, 1)
    king.attackingPositions(threeByThreeBoard).size shouldEqual 3
  }
}
