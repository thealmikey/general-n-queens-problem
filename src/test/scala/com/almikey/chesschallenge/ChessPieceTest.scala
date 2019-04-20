package com.almikey.chesschallenge

import org.scalatest._

class ChessPieceTest extends FlatSpec with Matchers {
  "A Blank slot" should "return false on attempt to capture any chess piece" in {
    import ChessPieces._
    var blank1: ChessPiece = new Blank((1, 1))
    var blank2: ChessPiece = new Blank((1, 2))
    assert(blank1.canCaptureOther(blank2) == false)
  }
}
