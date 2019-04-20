package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessBoard.Position
import com.almikey.chesschallenge.ChessPieces.{Blank, ChessPiece}

object ChessBoard {
  type ChessBoard = Vector[((Int, Int), ChessPiece)]
  type Position = (Int, Int)
  /*
We generate a board of n x m dimension and populate with Blank() pieces
which can't capture and basically represents an empty slot
   */
  def generateBoard(n: Int, m: Int): ChessBoard = {
    var chessBoardSeq: Vector[((Int, Int), ChessPiece)] =
      Vector.empty[((Int, Int), ChessPiece)]
    for (i <- 1 to n) {
      for (j <- 1 to m) {
        chessBoardSeq = chessBoardSeq.:+(((i, j), Blank((i, j))))
      }
    }
    chessBoardSeq
  }

  def placePieceOnBoard(chessBoard: ChessBoard,
                        chessPiece: ChessPiece,
                        position: Position): (ChessBoard, Boolean) = {
    var indexOfPosition = chessBoard.indexOf((position, Blank(position)))
    println(indexOfPosition)
    if (indexOfPosition == -1) {
      return (chessBoard, false)
    } else {
      println(chessBoard)
      var changedBoard =
        chessBoard.updated(indexOfPosition, (position, chessPiece))

      return (changedBoard, true)
    }
  }
}
