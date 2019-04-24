package com.almikey.chesschallenge

object ChessMain extends App {

  import com.almikey.chesschallenge.ChessBoard.ChessBoard
  import com.almikey.chesschallenge.ChessPieces._

  def calculateTimeTakenAndPrintResults(boardDimensions: BoardDimensions,
                                        f: () => Vector[ChessBoard]) = {
    def printResults(results: Vector[ChessBoard]) = {
      results.foreach(chessBoard => {
        println(ChessBoard.drawBoard(boardDimensions, chessBoard));
      })
    }
    var startTime = System.currentTimeMillis()
    var theans = f()
    var endTime = System.currentTimeMillis()
    printResults(theans)
    println("number of permutations:", theans.size)
    println("time take is:", (endTime - startTime), "ms")
  }

//  def normalizeInput(str: String): ((Int, Int), List[ChessPiece]) = {}
  type BoardDimensions = (Int, Int)
  def normalizeInput(
    myString: String
  ): Either[Throwable, (BoardDimensions, List[ChessPiece])] = {
    var inputArr = myString.split("board containing|board with|board that has")
    if (inputArr.size < 2) {
      Left(new Throwable("bad input from console"))
    } else {
      var firstPart = inputArr(0).trim
      var myBoardDimensionsArr =
        firstPart
          .split("x|X|×|by")
          .map(y => y.trim)
          .filter(!_.isEmpty)
          .map(_.toInt)
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
      "please input your request in this manner without quotes\n \"3x3 board with 2 Kings and 1 Rook\"\n type 'exit' if you want to leave program"
    )
    var userInput = readLine().trim
    if (userInput == "exit") {
      return
    }
    normalizeInput(userInput) match {
      case Right(value) => {
        var (boardDimen, chessPieces) = value
        var board = ChessBoard.generateBoard(boardDimen._1, boardDimen._2)
        var myPieces = chessPieces
        calculateTimeTakenAndPrintResults(boardDimen, () => {
          ChessConfigService.getAllPosibleConfigs(myPieces, board);
        })
        println("----------SUCCESS-----------")
        startMain()
      }
      case Left(value) => {
        println(value.getMessage);
        println("please try again")
        startMain()
      }
    }

  }
  startMain()
}
