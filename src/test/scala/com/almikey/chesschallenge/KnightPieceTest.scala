package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{ChessPiece, Knight}
import org.scalatest.{FlatSpec, Matchers}

class KnightPieceTest extends FlatSpec with Matchers {
  "A knight piece" should "have no positions to attack if the size of the board is less than 2 by 2" in {
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    var knight: ChessPiece = Knight(1, 1)
    knight.attackingPositions(twoByTwoBoard) shouldEqual Nil
  }
}
