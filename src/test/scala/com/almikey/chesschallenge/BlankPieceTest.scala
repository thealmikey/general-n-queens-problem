package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Blank, ChessPiece}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.Assertions._

class BlankPieceTest extends FlatSpec with Matchers {
  "A Blank slot" should "return false on attempt to capture any chess piece" in {
    import ChessPieces._
    var newBoard = ChessBoard.generateBoard(2, 3)
    var blank1: ChessPiece = new Blank()
    var blank2: ChessPiece = new Blank()
    assert(blank1.canCaptureOther(blank2, newBoard) == false)
  }
  "A Blank slot" should "not attack any position" in {
    var newBoard = ChessBoard.generateBoard(3, 3)
    //get a piece from the board which is initialized with Blanks
    var blankPiece: ChessPiece = newBoard(1)._2
    blankPiece.attackingPositions(newBoard) shouldEqual (List.empty[(Int, Int)])
  }
}
