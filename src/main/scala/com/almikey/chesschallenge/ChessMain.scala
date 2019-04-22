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

  def getAllPosibleConfigs(inputList: List[ChessPiece],
                           chessBoard: ChessBoard): Vector[ChessBoard] = {
    var results: Vector[ChessBoard] = Vector.empty[ChessBoard]
    var inputPermutations = createPermutationsOfInput(inputList)
    for (permutation <- inputPermutations) {
      results = results ++ shiftStartIndexVariations(permutation, chessBoard)
    }
    return results.distinct
  }

  def calculateTimeTakenAndPrintResults(f: () => Vector[ChessBoard]) = {
    def printResults(results: Vector[ChessBoard]) = {
      results.foreach(x => println(x + "\n"))
    }
    var startTime = System.currentTimeMillis()
    var theans = f()
    var endTime = System.currentTimeMillis()
    printResults(theans)
    println("number of permutations:", theans.size)
    println("time take is:", (endTime - startTime), "ms")
  }

//  def normalizeInput(str: String): ((Int, Int), List[ChessPiece]) = {}

  def normalizeInput(
      myString: String
  ): Either[Throwable, ((Int, Int), List[ChessPiece])] = {
    var inputArr = myString.split("board containing|board with|board that has")
    if (inputArr.size < 2) {
      Left(new Throwable("bad input from console"))
    } else {
      var firstPart = inputArr(0).trim
      var myBoardDimensionsArr =
        firstPart.split("x|X").map(y => y.trim).filter(!_.isEmpty).map(_.toInt)
      if (myBoardDimensionsArr.size < 2) {
        Left(new Throwable("bad input for dimensions"))
      } else {
        var myBoardDimenTuple =
          (myBoardDimensionsArr(0), myBoardDimensionsArr(1))
        var secondPart = inputArr(1).trim
        var rawPieces = secondPart.split("and|,").map(x => x.trim).map { y =>
          var splitSpace = y.split(" ")
          (splitSpace(0), splitSpace(1))
        }
        if (rawPieces.isEmpty) {
          Left(new Throwable("bad input from chess pieces"))
        } else {
          def stringToChessPiece(str: String): ChessPiece = {
            str.capitalize match {
              case "Rooks" | "Rook"     => Rook()
              case "Knights" | "Knight" => Knight()
              case "Kings" | "King"     => King()
              case "Bishops" | "Bishop" => Bishop()
              case "Queens" | "Queen"   => Queen()
              case _                    => Blank()
            }
          }

          var myNestedPieces = rawPieces.map { x =>
            var filler = x._1.toInt
            var thePiece = stringToChessPiece(x._2)
            List.fill(filler)(thePiece)
          }
          var myInputPieces =
            myNestedPieces.toList.flatten
          var withBadInputRemoved = myInputPieces.filter(!_.isInstanceOf[Blank])
          if (myInputPieces.size > withBadInputRemoved.size) {
            Left(new Throwable("bad input for one of the chess pieces"))
          } else {
            println(myBoardDimenTuple, myInputPieces)
            Right((myBoardDimenTuple, myInputPieces))
          }
        }
      }
    }
  }

  def startMain(): Unit = {
    println(
      "please input your request in this manner without quotes\n \"3x3 board containing 2 Kings and 1 Rook\"\n ::"
    )
    var userInput = readLine()
    normalizeInput(userInput) match {
      case Right(value) => {
        var (boardDimen, chessPieces) = value
        var board = ChessBoard.generateBoard(boardDimen._1, boardDimen._2)
        var myPieces = chessPieces
        calculateTimeTakenAndPrintResults(() => {
          getAllPosibleConfigs(myPieces, board);
        })
      }
      case Left(value) => { println(value.getMessage); startMain() }
    }

  }
  startMain()
}
