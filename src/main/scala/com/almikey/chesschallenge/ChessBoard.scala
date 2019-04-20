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

  /*
  We place a piece on the chess board. It can either succeed or fail,
   depending on some condition encapsulated in a method f. Returning Left(Chessboard) on failure
  and Right(Chessboard) to signify success
   */
  def placePieceOnBoard(
      chessBoard: ChessBoard,
      chessPiece: ChessPiece,
      position: Position,
      placesWeCantGo: Vector[(Int, Int)]
  ): Either[(ChessBoard, Vector[(Int, Int)]),
            (ChessBoard, Vector[(Int, Int)])] = {
    var indexOfPosition = chessBoard.indexOf((position, Blank(position)))
    //println(indexOfPosition)
    if (indexOfPosition == -1 && placesWeCantGo.contains(position)) {
      return Left((chessBoard, placesWeCantGo))
    } else {
      //println(chessBoard)
      var changedBoard: ChessBoard =
        chessBoard.updated(indexOfPosition, (position, chessPiece))
      var placesWeCantGo2 = placesWeCantGo
      placesWeCantGo2 =
        placesWeCantGo2.++(chessPiece.attackingPositions(changedBoard).toVector)
      // println(chessPiece.attackingPositions(changedBoard))
      return Right((changedBoard, placesWeCantGo2))
    }
  }

  def removePieceFromBoard(
      chessBoard: ChessBoard,
      chessPiece: ChessPiece,
      position: Position,
      placesWeCantGo: Vector[(Int, Int)]
  ): Either[(ChessBoard, Vector[(Int, Int)]),
            (ChessBoard, Vector[(Int, Int)])] = {
    var indexOfPosition = chessBoard.indexOf((position, chessPiece))
    // println(indexOfPosition)
    if (indexOfPosition == -1) {
      return Left((chessBoard, placesWeCantGo))
    } else {
      //println(chessBoard)
      var changedBoard: ChessBoard =
        chessBoard.updated(indexOfPosition, (position, Blank(position)))
      var placesWeCantGo2 = placesWeCantGo
      //      placesWeCantGo2 =
      //        placesWeCantGo2.++(chessPiece.attackingPositions(changedBoard).toVector)
      chessPiece.attackingPositions(changedBoard).foreach { x =>
        placesWeCantGo2 = dropFirstMatch(placesWeCantGo2, x).toVector
      }
      // println(chessPiece.attackingPositions(changedBoard))
      return Right((changedBoard, placesWeCantGo2))
    }
  }
//method to drop the first matching element from a Vector
  def dropFirstMatch[A](ls: Seq[A], value: A): Seq[A] = {
    val index = ls.indexOf(value) //index is -1 if there is no match
    if (index < 0) {
      ls
    } else if (index == 0) {
      ls.tail
    } else {
      // splitAt keeps the matching element in the second group
      val (a, b) = ls.splitAt(index)
      a ++ b.tail
    }
  }
}
