package SokobanGame

import SokobanGame.Sokoban._

class Game {
  def Play(board: Board): Unit = {
    def isGameWon(boxes: Set[Point]): Boolean = targets.subsetOf(boxes)

    while (!isGameWon(boxes)) {
      val move = scala.io.StdIn.readLine().charAt(0)
      if (moves.contains(move)) {
        moveStack.push((playerPos, boxes))
        val newPos = playerPos + moves(move)
        if (board(newPos.y)(newPos.x) != Wall) {
          if (boxes.contains(newPos)) {
            val newBoxPos = newPos + moves(move)
            if (board(newBoxPos.y)(newBoxPos.x) != Wall && !boxes.contains(newBoxPos)) {
              boxes = boxes - newPos + newBoxPos
              playerPos = newPos
            }
          } else {
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
      else {
        println("Neispravan unos!");
        return;
      }
    }
    if (isGameWon(boxes)) println("Pobeda!") else println("Block stanje?");
  }
}
