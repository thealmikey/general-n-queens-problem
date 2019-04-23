package com.almikey.chesschallenge

import com.almikey.chesschallenge.ChessBoard.ChessBoard
import com.almikey.chesschallenge.ChessPieces.ChessPiece

object ChessConfigService {
  /*
   We add pieces to the board and do backtracking if they're under attack from other pieces
   or can attack other pieces. We keep state of the pieces we've placed on the board
   by carrying the board data structure forward in the recursion and we also keep track on the
   remaining number of pieces yet to be placed in the chessPiecesList.
   Once we place a piece we track it's state by putting it in placedPieces list
   which we also carry forward in the recursion
   */
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
            } else if (placedPieces.isEmpty) {
              Vector.empty[((Int, Int), ChessPiece)]
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
  /*
It takes an input of chesspieces to be placed and a chessboard. The method places pieces on the
chessboard starting from index 0,placing and backtracking when needed until exhaustion of options
where it produces all possible permutations it could
do i.e. all possible permutations it could do after starting from index 0,so the next time it starts placing the pieces from index 1 and then after it's
exhausted the placing, it starts by placing the pieces from index 2 and increments by one till the first piece is placed
at the last slot of the board.
   */
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
  /*
This method produces the most possible combinations by using the permutation
and starting at different indices to produce the result of all the permutations possible
without attacking each other
   */
  def getAllPosibleConfigs(inputList: List[ChessPiece],
                           chessBoard: ChessBoard): Vector[ChessBoard] = {
    var results: Vector[ChessBoard] = Vector.empty[ChessBoard]
    var inputPermutations = createPermutationsOfInput(inputList)
    for (permutation <- inputPermutations) {
      results = results ++ shiftStartIndexVariations(permutation, chessBoard)
    }
    return results.distinct
  }
}
