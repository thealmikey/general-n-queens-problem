package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessBoard.ChessBoard

object ChessPieces {

  type PiecePosition = (Int, Int)

  sealed trait ChessPiece {
    def position: PiecePosition
    def canCaptureOther(chessPiece: ChessPiece): Boolean
    def attackingPositions(board: ChessBoard): List[(Int, Int)]
  }
  case class Blank(var position: PiecePosition = (0, 0)) extends ChessPiece {
    override def canCaptureOther(chessPiece: ChessPiece): Boolean = false

    override def attackingPositions(board: ChessBoard): List[(Int, Int)] = Nil
  }
}
