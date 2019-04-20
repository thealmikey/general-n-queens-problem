package com.almikey.chesschallenge

object ChessPieces {

  type PiecePosition = (Int, Int)

  sealed trait ChessPiece {
    def position: PiecePosition
    def canCaptureOther(chessPiece: ChessPiece): Boolean
  }
  case class Blank(var position: PiecePosition = (0, 0)) extends ChessPiece {
    override def canCaptureOther(chessPiece: ChessPiece): Boolean = false
  }
}
