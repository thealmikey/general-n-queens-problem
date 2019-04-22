package com.almikey.chesschallenge

object ChessMain extends App {

  import com.almikey.chesschallenge.ChessBoard.ChessBoard
  import com.almikey.chesschallenge.ChessPieces._

  def piecePlacerLoop(chessPiecesList: List[ChessPiece],
                      noGoZoneChecker: (ChessPiece) => Boolean,
                      index: Int,
                      chessBoard: ChessBoard,
                      placesWeCantGo: Vector[(Int, Int)],
                      placedPieces: List[ChessPiece]): ChessBoard = {
    chessPiecesList match {
      case x :: xs => {
        //        println(chessBoard)
        x.position = chessBoard(index)._1
        ChessBoard.placePieceOnBoard(
          chessBoard,
          x,
          chessBoard(index)._1,
          placesWeCantGo,
          ChessBoard.tellMeIfNoGoZoneMethodBuilder(placesWeCantGo, chessBoard)
        ) match {
          case Left(value) =>
            //            println("in the left", chessBoard.size)
            if (index + 1 < chessBoard.size) {
              piecePlacerLoop(
                chessPiecesList,
                noGoZoneChecker,
                index + 1,
                chessBoard,
                placesWeCantGo,
                placedPieces
              )
            } else {

              var mPrevIndex = chessBoard.indexOf(
                (placedPieces.head.position, placedPieces.head)
              )

              var chessPrev = ChessBoard
                .removePieceFromBoard(
                  chessBoard,
                  placedPieces.head,
                  placedPieces.head.position,
                  placesWeCantGo
                )
              var (prevBoard, prevNoGo) = chessPrev.merge
              if (mPrevIndex + 1 < chessBoard.length) {
                piecePlacerLoop(
                  placedPieces.head :: chessPiecesList,
                  noGoZoneChecker,
                  mPrevIndex + 1,
                  prevBoard,
                  prevNoGo,
                  placedPieces.tail
                )
              } else {
                Vector.empty[((Int, Int), ChessPiece)]
              }
            }
          case Right(value) =>
            // println(value._2)
            piecePlacerLoop(
              xs,
              noGoZoneChecker: (ChessPiece) => Boolean,
              0,
              value._1,
              value._2,
              x :: placedPieces
            )
        }
      }
      case Nil => chessBoard
    }
  }

  def createPermutationsOfInput(
    inputList: List[ChessPiece]
  ): List[List[ChessPiece]] = {
    inputList.permutations.toList
  }

  def shiftStartIndexVariations(inputList: List[ChessPiece],
                                chessBoard: ChessBoard): Vector[ChessBoard] = {
    var resultingConfigurations: Vector[ChessBoard] = Vector.empty[ChessBoard]
    for (i <- 0 to chessBoard.length - 1) {
      var placesWeCantGoOnBoard = Vector.empty[(Int, Int)]
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard,
          placesWeCantGoOnBoard,
          Nil
        )
      )
    }
    resultingConfigurations.filter(_.size > 0).distinct
  }
}
