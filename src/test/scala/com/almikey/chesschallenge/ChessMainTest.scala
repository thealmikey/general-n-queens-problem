package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Blank, King, Knight, Rook}
import org.scalatest.{FlatSpec, Matchers}

class ChessMainTest extends FlatSpec with Matchers {
  behavior of "ChessMainTest"

  "piecePlaceLooper" should "return a board with one piece with an input of 1 piece" in {
    var chessPieces = List(King())
    var chessBoard = ChessBoard.generateBoard(1, 1)
    var placesWeCantGoOnBoard = Vector.empty[(Int, Int)]
    var noGoZoneChecker =
      ChessBoard.tellMeIfNoGoZoneMethodBuilder(
        placesWeCantGoOnBoard,
        chessBoard
      )
    var startingIndex = 0
    var placePieces = Nil
    var result = ChessMain.piecePlacerLoop(
      chessPieces,
      noGoZoneChecker,
      startingIndex,
      chessBoard,
      placesWeCantGoOnBoard,
      placePieces
    )
    result shouldEqual Vector(((1, 1), King()))
  }
  it should "return Nil if we try to place 2 rooks in a 1 by 2 board" in {
    var chessPieces = List(Rook(), Rook())
    var chessBoard = ChessBoard.generateBoard(1, 2)
    var placesWeCantGoOnBoard = Vector.empty[(Int, Int)]
    var noGoZoneChecker =
      ChessBoard.tellMeIfNoGoZoneMethodBuilder(
        placesWeCantGoOnBoard,
        chessBoard
      )
    var startingIndex = 0
    var result = ChessMain.piecePlacerLoop(
      chessPieces,
      noGoZoneChecker,
      startingIndex,
      chessBoard,
      placesWeCantGoOnBoard,
      Nil
    )
    result shouldEqual Nil
  }

  it should "return a board with 2 knights on first column run when we try to place 2 knights in a 2 by 2 board" in {
    var chessPieces = List(Knight(), Knight())
    var chessBoard = ChessBoard.generateBoard(2, 2)
    var placesWeCantGoOnBoard = Vector.empty[(Int, Int)]
    var noGoZoneChecker =
      ChessBoard.tellMeIfNoGoZoneMethodBuilder(
        placesWeCantGoOnBoard,
        chessBoard
      )
    var startingIndex = 0
    var result = ChessMain.piecePlacerLoop(
      chessPieces,
      noGoZoneChecker,
      startingIndex,
      chessBoard,
      placesWeCantGoOnBoard,
      Nil
    )
    result shouldEqual Vector(((1, 1), Knight()),
                              ((1, 2), Knight()),
                              ((2, 1), Blank()),
                              ((2, 2), Blank()))
  }

}
