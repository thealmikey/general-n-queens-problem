package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Blank, ChessPiece}

object ChessBoard {
  type ChessBoard = Vector[(Int, Int, ChessPiece)]

  def generateBoard(n: Int, m: Int): ChessBoard = {
    var chessBoardSeq: Vector[((Int, Int, ChessPiece))] =
      Vector.empty[((Int, Int, ChessPiece))]
    for (i <- 1 to n) {
      for (j <- 1 to m) {
        chessBoardSeq = chessBoardSeq.:+(((i, j, Blank((i, j)))))
      }
    }
    chessBoardSeq
  }
}
