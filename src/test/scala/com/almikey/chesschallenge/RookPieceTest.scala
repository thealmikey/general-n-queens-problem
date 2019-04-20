package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{ChessPiece, Rook}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.Assertions._

class RookPieceTest extends FlatSpec with Matchers {
  "A Rook piece" should "should have no positions to attack if the board is 1 by 1" in {
    var oneByOneBoard = ChessBoard.generateBoard(1, 1)
    var rook: ChessPiece = Rook((1, 1), oneByOneBoard)
    rook.attackingPositions(oneByOneBoard) shouldEqual Nil
  }
  it should "attack 3 spaces in a 2 by 2 board" in {
    var threeByThreeBoard = ChessBoard.generateBoard(2, 2)
    var rook: ChessPiece = Rook((1, 1), threeByThreeBoard)
    println(rook.attackingPositions(threeByThreeBoard))
    rook.attackingPositions(threeByThreeBoard).size shouldEqual 3
  }
}
