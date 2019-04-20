package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessBoard.ChessBoard

object ChessPieces {

  type PiecePosition = (Int, Int)
  /*
    A chess Piece has a Board and a position on the board. It can calculate
    all the positions it can attack on its own board through myAttackingPositions
   */
  sealed trait ChessPiece {
    def board: ChessBoard.ChessBoard
    def position: PiecePosition
    def canCaptureOther(otherPiece: ChessPiece): Boolean = {
      var myAttackingPositions = attackingPositions(this.board)
      if (!otherPiece.isInstanceOf[Blank]) {
        if (myAttackingPositions.contains(
              (otherPiece.position._1, otherPiece.position._2)
            )) {
          return true
        } else {
          return false
        }
      } else {
        return false
      }
    }
    def attackingPositions(board: ChessBoard): List[(Int, Int)]
  }
  /*
We have a Blank piece as ChessPiece to represent an empty slot on the board. It can't capture other pieces
and doesn't have positions it can  attack
   */
  case class Blank(var position: PiecePosition = (0, 0),
                   var board: ChessBoard = Vector.empty)
      extends ChessPiece {
    override def canCaptureOther(chessPiece: ChessPiece): Boolean = false

    override def attackingPositions(board: ChessBoard): List[(Int, Int)] = Nil
  }

  case class Knight(var position: PiecePosition, var board: ChessBoard)
      extends ChessPiece {
    override def canCaptureOther(otherPiece: ChessPiece): Boolean = {
      var myAttackingPositions = attackingPositions(this.board)
      if (!otherPiece.isInstanceOf[Blank]) {
        if (myAttackingPositions.contains(
              (otherPiece.position._1, otherPiece.position._2)
            )) {
          return true
        } else {
          return false
        }
      } else {
        return false
      }
    }

    override def attackingPositions(board: ChessBoard): List[(Int, Int)] = {
      if (board.length > 2) {
        var blackList = List.empty[(Int, Int)]
        var move1Box = List(1, -1)
        var move2Box = List(2, -2)

        var move1Then2 = for {
          n <- move1Box
          m <- move2Box
        } yield (n, m)
        var move2Then1 = for {
          n <- move2Box
          m <- move1Box
        } yield (n, m)

        var legalMoves = (move1Then2 ++ move2Then1).distinct

        var allPositions = for {
          n <- legalMoves
        } yield {
          var pos1 = position._1 + n._1
          var pos2 = position._2 + n._2
          if (pos1 > 0 && pos2 > 0) {
            (pos1, pos2)
          } else {
            (0, 0)
          }
        }
        var boardPositions = board.map(x => x._1)
        for (m <- allPositions) {
          if (m._1 != 0 && m._2 != 0 && boardPositions.indexOf((m._1, m._2)) != -1) {
            blackList :+= (m._1, m._2)

          }
        }
        blackList
      } else {
        Nil
      }
    }
  }

  case class Bishop(var position: (Int, Int), board: ChessBoard)
      extends ChessPiece
      with DiagonalAttackTrait {
    override def canCaptureOther(chessPiece: ChessPiece): Boolean = ???

    override def attackingPositions(board: ChessBoard): List[(Int, Int)] =
      diagonalAttackingPositions(this, board)
  }

}
