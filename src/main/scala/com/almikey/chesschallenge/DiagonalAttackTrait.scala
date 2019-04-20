package com.almikey.chesschallenge

import ChessPieces.ChessPiece
import ChessBoard._

trait DiagonalAttackTrait {
  this: ChessPiece =>
  def diagonalAttackingPositions(
      chessPiece: ChessPiece,
      completeBoard: ChessBoard
  ): List[(Int, Int)] = {
    var board = completeBoard.map(x => x._1)
    var blackList = List.empty[(Int, Int)]
    for (n <- board) {
      if ((n._2 - n._1 == chessPiece.position._2 - chessPiece.position._1
          || n._2 + n._1 == chessPiece.position._2 + chessPiece.position._1) && (
            n._1,
            n._2
          ) != this.position) {
        blackList :+= (n._1, n._2)
        println(this.position == (n._1, n._2))
      }
    }
    blackList
  }
}
