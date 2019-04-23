package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces.{Blank, King, Knight, Queen, Rook}
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
  it should "return a board with 2 knights on first column run when place 2 knights in a 2 by 2 board once" in {
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
    result shouldEqual Vector(
      ((1, 1), Knight()),
      ((1, 2), Knight()),
      ((2, 1), Blank()),
      ((2, 2), Blank())
    )
  }
  "creatingPermutations method given a chess list with 1 item" should "return a list of size 1 item" in {
    var chessPiecesList = List(Queen())
    ChessMain.createPermutationsOfInput(chessPiecesList).size shouldEqual 1
  }
  "creatingPermutations method given a list with 1 item" should "return a list of size 1 item" in {
    var chessPiecesList = List(Queen())
    ChessMain.createPermutationsOfInput(chessPiecesList).size shouldEqual 1
  }
  "get allPossibleConfig method given a 3x3 board containing 2 Kings and 1 Rook" should "return 4 unique configurations" in {
    var chessPiecesList = List(King(), King(), Rook())
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    ChessMain
      .getAllPosibleConfigs(chessPiecesList, threeByThreeBoard)
      .size shouldEqual 4
  }
  "get allPossibleConfig method given a 4x4 board containing 2 Rooks and 4 Knights" should "return 4 unique configurations" in {
    var chessPiecesList =
      List(Rook(), Rook(), Knight(), Knight(), Knight(), Knight())
    var fourByFourBoard = ChessBoard.generateBoard(4, 4)
    ChessMain
      .getAllPosibleConfigs(chessPiecesList, fourByFourBoard)
      .size shouldEqual 8
  }

  "get allPossibleConfig method given a 2x2 board containing 2 Kings" should "return 0 possible configurations" in {
    var chessPiecesList = List(King(), King())
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    ChessMain
      .getAllPosibleConfigs(chessPiecesList, twoByTwoBoard)
      .size shouldEqual 0
  }

  "normalizeInput method given a text '4x4 board containing 2 Rooks and 4 Knights'" should "return a tuple with board dimensions and a list of chesspieces" in {
    var expectedChessPieceList =
      List(Rook(), Rook(), Knight(), Knight(), Knight(), Knight())
    var expectedBoardDimensions = (4, 4)
    ChessMain
      .normalizeInput("4x4 board containing 2 Rooks and 4 Knights")
      .merge shouldEqual ((expectedBoardDimensions, expectedChessPieceList))
  }

}
