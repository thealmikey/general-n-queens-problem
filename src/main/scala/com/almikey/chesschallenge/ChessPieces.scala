package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessBoard.ChessBoard
import com.almikey.chesschallenge.SharedChessPieceTraits.{
  DiagonalAttackTrait,
  HorizontalVerticalAttackTrait
}

object ChessPieces {

  type PiecePosition = (Int, Int)
  /*
    A chess Piece has a Board and a position on the board. It can calculate
    all the positions it can attack on its own board through the attackingPositions()
   */
  sealed trait ChessPiece {
    var board: ChessBoard.ChessBoard = _
    var position: PiecePosition = _
    def canCaptureOther(otherPiece: ChessPiece, board: ChessBoard): Boolean = {
      var myAttackingPositions = attackingPositions(board)
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
  case class Blank() extends ChessPiece {
    override def canCaptureOther(chessPiece: ChessPiece,
                                 board: ChessBoard): Boolean = false

    override def attackingPositions(board: ChessBoard): List[(Int, Int)] = Nil

    override def toString = "0"
  }

  case class Knight() extends ChessPiece {
    override def canCaptureOther(otherPiece: ChessPiece,
                                 board: ChessBoard): Boolean = {
      var myAttackingPositions = attackingPositions(board)
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

    override def toString: String = "N"
  }

  case class Bishop() extends ChessPiece with DiagonalAttackTrait {

    override def attackingPositions(board: ChessBoard): List[(Int, Int)] =
      diagonalAttackingPositions(this, board)

    override def toString: String = "B"
  }

  case class Rook() extends ChessPiece with HorizontalVerticalAttackTrait {
    override def attackingPositions(board: ChessBoard): List[(Int, Int)] =
      verticalHorizontalAttackingPositions(this, board)

    override def toString: String = "R"
  }

  case class Queen()
      extends ChessPiece
      with HorizontalVerticalAttackTrait
      with DiagonalAttackTrait {
    override def attackingPositions(board: ChessBoard): List[(Int, Int)] =
      diagonalAttackingPositions(this, board) ++ verticalHorizontalAttackingPositions(
        this,
        board
      )

    override def toString: String = "Q"
  }

  case class King() extends ChessPiece {
    override def attackingPositions(board: ChessBoard): List[(Int, Int)] = {
      if (board.length > 2) {
        var blackList = List.empty[(Int, Int)]
        var moveCombo1 = List(1, -1)
        var moveCombo2 = List(-1, 1)
        var moveCombo3 = List(0, 1)
        var moveCombo4 = List(-1, 0)

        var move1Then2 = for {
          n <- moveCombo1
          m <- moveCombo2
        } yield (n, m)
        var move2Then1 = for {
          n <- moveCombo2
          m <- moveCombo1
        } yield (n, m)
        var move3Then4 = for {
          n <- moveCombo3
          m <- moveCombo4
        } yield (n, m)
        var move4Then3 = for {
          n <- moveCombo4
          m <- moveCombo3
        } yield (n, m)

        var legalMoves =
          (move1Then2 ++ move2Then1 ++ move3Then4 ++ move4Then3).distinct
//        println(legalMoves)
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
          if ((m._1 != 0 && m._2 != 0 && boardPositions.indexOf((m._1, m._2)) != -1) && (
                m._1,
                m._2
              ) != this.position) {
            blackList :+= (m._1, m._2)

          }
        }
        blackList
      } else {
        Nil
      }
    }
    override def toString: String = "K"
  }

}
