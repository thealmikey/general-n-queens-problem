package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Bishop, ChessPiece}
import org.scalatest.{FlatSpec, Matchers}

class BishopPieceTest extends FlatSpec with Matchers {
  "A Bishop piece" should "should have no positions to attack if the board is 1 by 1 or 1 by 2" in {
    var oneByOneBoard = ChessBoard.generateBoard(1, 1)
    var oneByTwoBoard = ChessBoard.generateBoard(1, 2)
    var bishop: ChessPiece = Bishop()
    bishop.position = (1, 1)
    bishop.attackingPositions(oneByOneBoard) shouldEqual Nil
    bishop.attackingPositions(oneByTwoBoard) shouldEqual Nil
  }
  it should "attack 1 spaces in a 2 by 2 board" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var bishop: ChessPiece = Bishop()
    bishop.position = (1, 1)
    var expectedAttackingPositions = Vector((2, 2))
    bishop.attackingPositions(twoByTwoBoard).size shouldEqual 1
    bishop.attackingPositions(twoByTwoBoard) shouldEqual expectedAttackingPositions
  }
}
