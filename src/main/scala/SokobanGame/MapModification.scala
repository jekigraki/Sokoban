package SokobanGame

import SokobanGame.Sokoban.{Board, boxes, moves, playerPos, targets}

class MapModification {
  val mapValidation = new MapValidation();
  def removeFirstRow(board: Board): Board = {
    val numRows = board.length
    val numCols = board(0).length
    val newBoard = Array.ofDim[Tile](numRows - 1, numCols)
    if (board.length > 1) {
      for (i <- 1 until board.length) {
        newBoard(i - 1) = board(i)
      }
    }
    for (box <- boxes) {
      boxes = boxes - box + new Point(box.x, box.y - 1)
    }
    for (target <- targets) {
      targets = targets - target + new Point(target.x, target.y - 1)
    }

    val newPos = Point(playerPos.x, playerPos.y - 1)
    playerPos = newPos;
    newBoard
  }

  def removeLastRow(board: Board): Board = {
    val numRows = board.length
    val numCols = board(0).length
    var newBoard = Array.ofDim[Tile](numRows - 1, numCols)
    if (board.length > 1) {
      newBoard = board.dropRight(1)
    }
    newBoard
  }

  def removeFirstColumn(board: Board): Board = {
    val numRows = board.length
    val numCols = board(0).length
    val newBoard = Array.ofDim[Tile](numRows, numCols - 1)
    if (board(0).length > 1) {
      for (i <- board.indices) {
        newBoard(i) = board(i).tail
      }
    }
    for (box <- boxes) {
      boxes = boxes - box + new Point(box.x -1, box.y)
    }
    for (target <- targets) {
      targets = targets - target + new Point(target.x - 1, target.y)
    }

    val newPos = Point(playerPos.x -1, playerPos.y)
    playerPos = newPos;
    newBoard
  }

  def removeLastColumn(board: Board): Board = {
    val numRows = board.length
    val numCols = board(0).length
    val newBoard = Array.ofDim[Tile](numRows, numCols - 1)
    if (board(0).length > 1) {
      for (i <- board.indices) {
        newBoard(i) = board(i).dropRight(1)
      }
    }
    newBoard
  }

  def extendRow(board: Board): Board = {
    val numRows = board.length;
    val numCols = board(0).length;
    println("Unesite da li želite prvi ili poslednji red da proširite:");
    val position = scala.io.StdIn.readLine();
    var pos = numRows;
    if(position == "prvi")
      {
        pos = 0;
        playerPos = playerPos.copy(y = playerPos.y + 1)
        for (box <- boxes) {
          boxes = boxes - box + new Point(box.x, box.y + 1)
        }
        for (target <- targets) {
          targets = targets - target + new Point(target.x, target.y + 1)
        }
      }

    println("Unesite niz karaktera dužine "  + (numCols) + ':');
    val newRow = scala.io.StdIn.readLine();
    //regex
    val newBoard = Array.ofDim[Tile](numRows + 1, numCols);
    for (x <- 0 until numCols) {
      newBoard(pos)(x) = newRow.charAt(x) match {
        case '#' => Wall
        case '-' => Space
        case '.' => Target
        case 'x' => Box
        case 's' => Player
        case _ => Space
      }
      newRow.charAt(x) match {
        case '.' => {
          targets += Point(x, pos)
        }
        case 'x' => {
          boxes += Point(x, pos)
        }
        case _ => ()
      }
    }
    if (position == "prvi") {
      for (x <- 1 until numRows + 1) {
        for (y <- 0 until numCols) {
          newBoard(x)(y) = board(x - 1)(y)
        }
      }
    }else {
      for (x <- 0 until numRows) {
        for (y <- 0 until numCols) {
          newBoard(x)(y) = board(x)(y)
        }
      }
    }
    newBoard
  }

  def extendFirstColumn(board: Board,  userInput: Option[String]= None): Board = {
    val numRows = board.length;
    val numCols = board(0).length;
    println("Unesite niz karaktera dužine " + (numRows) + ':');
    val newColumn = userInput.getOrElse(scala.io.StdIn.readLine());

    val newBoard = Array.ofDim[Tile](numRows, numCols + 1)

    for (box <- boxes) {
      boxes = boxes - box + new Point(box.x + 1, box.y)
    }
    for (target <- targets) {
      targets = targets - target + new Point(target.x + 1, target.y)
    }
    playerPos = playerPos.copy(x = playerPos.x + 1)

    for (y <- 0 until numRows) {
      newBoard(y)(0) = newColumn.charAt(y) match {
        case '#' => Wall
        case '-' => Space
        case '.' => Target
        case 'x' => Box
        case 's' => Player
        case _ => Space
      }
      newColumn.charAt(y) match {
        case '.' => {
          targets += Point(0, y)
        }
        case 'x' => {
          boxes += Point(0, y)
        }
        case _ => ()
      }
    }

    for (x <- 0 until numRows) {
      for (y <- 0 until numCols) {
        newBoard(x)(y+1) = board(x)(y)
      }
    }

    newBoard
  }

  def extendLastColumn(board: Board, userInput: Option[String]= None): Board = {
    val numRows = board.length
    val numCols = board(0).length

    println("Unesite niz karaktera dužine " + (numRows) + ':');
    val newColumn = userInput.getOrElse(scala.io.StdIn.readLine());

    val newBoard = Array.ofDim[Tile](numRows, numCols + 1)

    for (y <- 0 until numRows) {
      for (x <- 0 until numCols) {
        newBoard(y)(x) = board(y)(x)
      }
    }

    for (y <- 0 until numRows) {
      newBoard(y)(numCols) = newColumn.charAt(y) match {
        case '#' => Wall
        case '-' => Space
        case '.' => Target
        case 'x' => Box
        case 's' => Player
        case _ => Space
      }
      newColumn.charAt(y) match {
        case '.' => {
          targets += Point(numCols, y)
        }
        case 'x' => {
          boxes += Point(numCols, y)
        }
        case _ => ()
      }
    }
    newBoard
  }

  def replaceTileAtPosition(board: Board): Board = {

    // stari boxes/targets?
    println("Unesite koordinatu x:")
    val x = scala.io.StdIn.readInt();
    println("Unesite koordinatu y:")
    val y = scala.io.StdIn.readInt();
    println("Unesite novi tip polja:")
    val newTile = scala.io.StdIn.readLine().charAt(0);

    board(x)(y)  match {
      case Target => {
        targets -= Point(x, y)
      }
      case Box => {
        boxes -= Point(x, y)
      }
      case _ => ()
    }


    if (x >= 0 && x < board.length && y >= 0 && y < board(0).length) {
      board(x)(y) = newTile match {
        case '#' => Wall
        case '-' => Space
        case '.' => {
          targets += Point(x, y)
          Target
        }
        case 'x' => {
          boxes += Point(x, y)
          Box
        }
        case 's' => {
          playerPos = Point(x, y)
          Player
        }
      }
    }
    board
  }

  def invertBoard(board: Board): Board = {
    val invertedBoard = Array.ofDim[Tile](board.length, board(0).length)

    for (x <- board.indices; y <- board(x).indices) {
      board(x)(y) match {
        case Box => invertedBoard(x)(y) = Target
        case Target => invertedBoard(x)(y) = Box
        case _ => invertedBoard(x)(y) = board(x)(y)
      }
    }

    var temp = targets;
    targets = boxes;
    boxes = temp;

    invertedBoard
  }

  def filterBoard(board: Board):Board = {
    val numRows = board.length
    val numCols = board(0).length

    val filteredBoard = Array.ofDim[Tile](numRows, numCols)
    for (y <- 0 until numRows) {
      for (x <- 0 until numCols) {
        filteredBoard(y)(x) = board(y)(x)
      }
    }

    println("Unesite koordinatu x:")
    val x = scala.io.StdIn.readInt();
    println("Unesite koordinatu y:")
    val y = scala.io.StdIn.readInt();
    println("Unesite N:")
    val N = scala.io.StdIn.readInt();

    def isSurroundedByWall(x: Int, y: Int): Boolean = {
      var i = 1
      while(i <= N) {
        val dx = Array(-i, 0, i, 0)
        val dy = Array(0, -i, 0, i)

        for (j <- 0 until 4) {
          val newX = x + dx(j)
          val newY = y + dy(j)

          if (newX >= 0 && newX < numCols && newY >= 0 && newY < numRows) {
            if (board(newY)(newX) == Wall) {
              return true
            }
          }
        }
        i = i + 1;
      }
      false
    }

    if(isSurroundedByWall(x, y)) {
      filteredBoard(x)(y) = Space
    }
    filteredBoard
  }

  def wallMinimization(board: Board) : Board = {
    val outerWalls = mapValidation.getEnclosingWalls(board);
    val numRows = board.length;
    val numCols = board(0).length;
    for (x <- 0 until numRows) {
      for (y <- 0 until numCols) {
        if(board(x)(y) == Wall) {
            for(i <- outerWalls.indices) {
                if((outerWalls(i)._1 == x) && (outerWalls(i)._2 == y)){
                  board(x)(y) = Space
                }
              }
          }
      }
    }
    board
  }

  def SetBoxes(myBoxes: Set[Point]): Unit = {
    boxes = myBoxes;
  }

  def SetTargets(myTargets: Set[Point]): Unit = {
    targets = myTargets;
  }

  def SetPlayerPosition(myPlayerPosition: Point): Unit = {
    playerPos = myPlayerPosition;
  }
}