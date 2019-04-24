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
   which we also carry forward in the recursion.
   -startPlacingFromStart argument is a boolean that directs our recursion when we backtrack,
    if true the backtracking algorithm will try to place a piece in the first index of the board
   if false it will try placing the piece right after the last index of the previously placed piece.
   */
  def piecePlacerLoop(chessPiecesList: List[ChessPiece],
                      noGoZoneChecker: (ChessPiece) => Boolean,
                      index: Int,
                      chessBoard: ChessBoard,
                      placesWeCantGo: Vector[(Int, Int)],
                      placedPieces: List[ChessPiece],
                      startPlacingFromStart: Boolean): ChessBoard = {
    chessPiecesList match {
      case x :: xs => {
        x.position = chessBoard(index)._1
        ChessBoard.placePieceOnBoard(
          chessBoard,
          x,
          chessBoard(index)._1,
          placesWeCantGo,
          ChessBoard.tellMeIfNoGoZoneMethodBuilder(placesWeCantGo, chessBoard)
        ) match {
          case Left(value) =>
            if (index + 1 < chessBoard.size) {
              piecePlacerLoop(
                chessPiecesList,
                noGoZoneChecker,
                index + 1,
                chessBoard,
                placesWeCantGo,
                placedPieces,
                startPlacingFromStart
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
                  placedPieces.tail,
                  startPlacingFromStart
                )
              } else {
                Vector.empty[((Int, Int), ChessPiece)]
              }
            }
          case Right(value) =>
            if (startPlacingFromStart) {
              piecePlacerLoop(
                xs,
                noGoZoneChecker: (ChessPiece) => Boolean,
                0,
                value._1,
                value._2,
                x :: placedPieces,
                startPlacingFromStart
              )
            } else {
              var newBoardPieces = value._1.splitAt(index)
              var newBoard = newBoardPieces._2 ++ newBoardPieces._1
              piecePlacerLoop(
                xs,
                noGoZoneChecker: (ChessPiece) => Boolean,
                0,
                newBoard,
                value._2,
                x :: placedPieces,
                true
              )
            }

        }
      }
      case Nil => chessBoard.sortBy(x => (x._1._1, x._1._2))
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
    //start placing pieces from the start of the board, try adding pieces from the first index 0 and increment from there
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
          Nil,
          true
        )
      )
      //start placing pieces from the index of piece last placed. If last piece was placed at index 5 try index 6
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard,
          placesWeCantGoOnBoard,
          Nil,
          false
        )
      )
      //start placing pieces incrementally from the opposite side of the board e.g. position(8,8) then (8,7)
      //incrementing by 1 all the way to the front to (1,1)
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard.reverse,
          placesWeCantGoOnBoard,
          Nil,
          true
        )
      )
      //tart placing pieces incrementally from the opposite side of the board, but instead of trying to keep starting a new
      // placement from the very end e.g. in a 8x8 board ,trying to add a piece right at (8,8) then (8,7),
      // place pieces from the index of the piece last placed.
      // If last piece was placed at index (8,6) try index (8,5) next as you decrement to the start of the board
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard.reverse,
          placesWeCantGoOnBoard,
          Nil,
          false
        )
      )

      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard
            .splitAt(chessBoard.size / 2)
            ._2 ++ chessBoard.splitAt(chessBoard.size / 2)._1.reverse,
          placesWeCantGoOnBoard,
          Nil,
          false
        )
      )
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard
            .splitAt(chessBoard.size / 2)
            ._2 ++ chessBoard.splitAt(chessBoard.size / 2)._1.reverse,
          placesWeCantGoOnBoard,
          Nil,
          true
        )
      )
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard
            .splitAt(chessBoard.size / 2)
            ._2
            .reverse ++ chessBoard.splitAt(chessBoard.size / 2)._1,
          placesWeCantGoOnBoard,
          Nil,
          false
        )
      )
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard
            .splitAt(chessBoard.size / 2)
            ._2
            .reverse ++ chessBoard.splitAt(chessBoard.size / 2)._1,
          placesWeCantGoOnBoard,
          Nil,
          true
        )
      )

      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard
            .splitAt(chessBoard.size / 2)
            ._2
            ++ chessBoard.splitAt(chessBoard.size / 2)._1,
          placesWeCantGoOnBoard,
          Nil,
          false
        )
      )
      resultingConfigurations = resultingConfigurations.:+(
        piecePlacerLoop(
          inputList,
          ChessBoard
            .tellMeIfNoGoZoneMethodBuilder(placesWeCantGoOnBoard, chessBoard),
          i,
          chessBoard
            .splitAt(chessBoard.size / 2)
            ._2
            ++ chessBoard.splitAt(chessBoard.size / 2)._1,
          placesWeCantGoOnBoard,
          Nil,
          true
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
