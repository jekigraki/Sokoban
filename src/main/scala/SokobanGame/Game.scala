package SokobanGame

import SokobanGame.Sokoban._

import scala.collection.immutable.Set
import scala.collection.mutable
import scala.io.Source




class Game (){
  def Play(board: Board, userInput: Option[String]= None): Unit = {
    def isGameWon(boxes: Set[Point]): Boolean = targets.subsetOf(boxes)
    printBoard(board)
    while (!isGameWon(boxes)) {
      val move = userInput.getOrElse(scala.io.StdIn.readLine()).charAt(0)
      if (moves.contains(move)) {
        moveStack.push((playerPos, boxes))
        val newPos = playerPos + moves(move)
        if (board(newPos.y)(newPos.x) != Wall) {
          if (boxes.contains(newPos)) {
            val newBoxPos = newPos + moves(move)
            if (board(newBoxPos.y)(newBoxPos.x) != Wall && !boxes.contains(newBoxPos)) {
              boxes = boxes - newPos + newBoxPos
              if(targets.contains(playerPos)) {
                board(playerPos.y)(playerPos.x) = Target
              }
              else{
                board(playerPos.y)(playerPos.x) = Space
              }
              playerPos = newPos
            }
          } else {
            if (targets.contains(playerPos)) {
              board(playerPos.y)(playerPos.x) = Target
            }
            else {
              board(playerPos.y)(playerPos.x) = Space
            }
            playerPos = newPos
          }
        }
        printBoard(board)
      }
      else if (move == 'z' && moveStack.nonEmpty) {
        val (prevPlayerPos, prevBoxes) = moveStack.pop()
        playerPos = prevPlayerPos
        boxes = prevBoxes
        printBoard(board)
      }
      else if (move == 'x') {
        println("Prekid igre.");
        return
      }
      else {
        println("Neispravan unos!");
        return;
      }
    }
    if (isGameWon(boxes)) println("Pobeda!") else println("Blokirano stanje!");
  }

  def PlayFromFile(board: Board): Unit = {
    val movesArray = Source.fromFile("C:\\games\\moves\\output.txt").getLines().toArray
    def isGameWon(boxes: Set[Point]): Boolean = targets.subsetOf(boxes)
    printBoard(board)
    var i = 0;
    while (!isGameWon(boxes) && i < movesArray.length) {
      val move = movesArray(i).charAt(0);
      i = i + 1;
      if (moves.contains(move)) {
        moveStack.push((playerPos, boxes))
        val newPos = playerPos + moves(move)
        if (board(newPos.y)(newPos.x) != Wall) {
          if (boxes.contains(newPos)) {
            val newBoxPos = newPos + moves(move)
            if (board(newBoxPos.y)(newBoxPos.x) != Wall && !boxes.contains(newBoxPos)) {
              boxes = boxes - newPos + newBoxPos
              if (targets.contains(playerPos)) {
                board(playerPos.y)(playerPos.x) = Target
              }
              else {
                board(playerPos.y)(playerPos.x) = Space
              }
              playerPos = newPos
            }
          } else {
            if (targets.contains(playerPos)) {
              board(playerPos.y)(playerPos.x) = Target
            }
            else {
              board(playerPos.y)(playerPos.x) = Space
            }
            playerPos = newPos
          }
        }
        printBoard(board)
      }
      else if (move == 'z' && moveStack.nonEmpty) {
        val (prevPlayerPos, prevBoxes) = moveStack.pop()
        playerPos = prevPlayerPos
        boxes = prevBoxes
        printBoard(board)
      }
      else if (move == 'x') {
        println("Prekid igre.");
        return
      }
      else {
        println("Neispravan unos!");
        return;
      }
    }
    if (isGameWon(boxes)) println("Pobeda!") else println("Block stanje?");
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

  def ResetStack(): Unit = {
    moveStack = mutable.Stack.empty;
  }

  def SetMoves(): Unit = {
      moves = Map(
      'u' -> Point(0, -1),
      'd' -> Point(0, 1),
      'l' -> Point(-1, 0),
      'r' -> Point(1, 0)
    )
  }
}
