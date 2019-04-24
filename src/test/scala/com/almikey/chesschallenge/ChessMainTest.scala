package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessPieces._
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
    var result = ChessConfigService.piecePlacerLoop(
      chessPieces,
      noGoZoneChecker,
      startingIndex,
      chessBoard,
      placesWeCantGoOnBoard,
      placePieces,
      true
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
    var result = ChessConfigService.piecePlacerLoop(
      chessPieces,
      noGoZoneChecker,
      startingIndex,
      chessBoard,
      placesWeCantGoOnBoard,
      Nil,
      true
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
    var result = ChessConfigService.piecePlacerLoop(
      chessPieces,
      noGoZoneChecker,
      startingIndex,
      chessBoard,
      placesWeCantGoOnBoard,
      Nil,
      true
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
    ChessConfigService
      .createPermutationsOfInput(chessPiecesList)
      .size shouldEqual 1
  }
  "creatingPermutations method given a list with 1 item" should "return a list of size 1 item" in {
    var chessPiecesList = List(Queen())
    ChessConfigService
      .createPermutationsOfInput(chessPiecesList)
      .size shouldEqual 1
  }
  "get allPossibleConfig method given a 3x3 board containing 2 Kings and 1 Rook" should "return 4 unique configurations" in {
    var chessPiecesList = List(King(), King(), Rook())
    var threeByThreeBoard = ChessBoard.generateBoard(3, 3)
    ChessConfigService
      .getAllPosibleConfigs(chessPiecesList, threeByThreeBoard)
      .size shouldEqual 4
  }
  "get allPossibleConfig method given a 4x4 board containing 2 Rooks and 4 Knights" should "return 4 unique configurations" in {
    var chessPiecesList =
      List(Rook(), Rook(), Knight(), Knight(), Knight(), Knight())
    var fourByFourBoard = ChessBoard.generateBoard(4, 4)
    ChessConfigService
      .getAllPosibleConfigs(chessPiecesList, fourByFourBoard)
      .size shouldEqual 8
  }

  "get allPossibleConfig method given a 2x2 board containing 2 Kings" should "return 0 possible configurations" in {
    var chessPiecesList = List(King(), King())
    var twoByTwoBoard = ChessBoard.generateBoard(2, 2)
    ChessConfigService
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

  "getAllPossibleConfigs given 4 Queens on a 4x4 board" should "return 2 permutations" in {
    var rawInput = "4x4 board containing 4 queens";
    var (boardDimension: (Int, Int), chessPiecesList: List[ChessPiece]) =
      ChessMain.normalizeInput(rawInput).merge
    var chessBoard =
      ChessBoard.generateBoard(boardDimension._1, boardDimension._2)
    ChessConfigService
      .getAllPosibleConfigs(chessPiecesList, chessBoard)
      .size shouldEqual 2
  }
  "getAllPossibleConfigs given 5 Queens on a 5x5 board" should "return 10 permutations" in {
    var rawInput = "5x5 board containing 5 queens";
    var (boardDimension: (Int, Int), chessPiecesList: List[ChessPiece]) =
      ChessMain.normalizeInput(rawInput).merge
    var chessBoard =
      ChessBoard.generateBoard(boardDimension._1, boardDimension._2)
    ChessConfigService
      .getAllPosibleConfigs(chessPiecesList, chessBoard)
      .size shouldEqual 10
  }
  "getAllPossibleConfigs given 6 Queens on a 6x6 board" should "return 4 permutations" in {
    var rawInput = "6x6 board containing 6 queens";
    var (boardDimension: (Int, Int), chessPiecesList: List[ChessPiece]) =
      ChessMain.normalizeInput(rawInput).merge
    var chessBoard =
      ChessBoard.generateBoard(boardDimension._1, boardDimension._2)
    ChessConfigService
      .getAllPosibleConfigs(chessPiecesList, chessBoard)
      .size shouldEqual 4
  }
  "getAllPossibleConfigs given 7 Queens on a 7x7 board" should "return 40 permutations" in {
    var rawInput = "7x7 board containing 7 queens";
    var (boardDimension: (Int, Int), chessPiecesList: List[ChessPiece]) =
      ChessMain.normalizeInput(rawInput).merge
    var chessBoard =
      ChessBoard.generateBoard(boardDimension._1, boardDimension._2)
    ChessConfigService
      .getAllPosibleConfigs(chessPiecesList, chessBoard)
      .size shouldEqual 40
  }
}
