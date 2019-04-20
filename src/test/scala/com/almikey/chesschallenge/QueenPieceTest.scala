package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{ChessPiece, Queen}
import org.scalatest.{FlatSpec, Matchers}

class QueenPieceTest extends FlatSpec with Matchers {
  "A Queen piece" should "should have no positions to attack if the board is 1 by 1" in {
    var oneByOneBoard = ChessBoard.generateBoard(1, 1)
    var queen: ChessPiece = Queen((1, 1), oneByOneBoard)
    queen.attackingPositions(oneByOneBoard) shouldEqual Nil
  }

  it should "attack 3 spaces in a 2 by 2 board" in {
    var threeByThreeBoard = ChessBoard.generateBoard(2, 2)
    var queen: ChessPiece = Queen((1, 1), threeByThreeBoard)
    queen.attackingPositions(threeByThreeBoard).size shouldEqual 3
  }
}
