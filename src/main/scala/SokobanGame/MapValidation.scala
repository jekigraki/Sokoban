package SokobanGame

import SokobanGame.Sokoban.{Board, playerPos}

class MapValidation {
  def findPlayerPosition(board: Board): Option[(Int, Int)] = {
    for (i <- board.indices; j <- board(i).indices if board(i)(j) == 's') {
      return Some((i, j))
    }
    None
  }

  def isWallsEnclosing(board: Board): Boolean = {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

    val playerPos = findPlayerPosition(board).getOrElse(return false)

    val visited = Array.fill(board.length, board(0).length)(false)

    var stack: List[(Int, Int)] = List(playerPos)

    while (stack.nonEmpty) {
      val (i, j) = stack.head
      stack = stack.tail

      if (!visited(i)(j) && board(i)(j) != '#') {
        visited(i)(j) = true

        for ((di, dj) <- directions) {
          val (ni, nj) = (i + di, j + dj)
          if (ni >= 0 && ni < board.length && nj >= 0 && nj < board(ni).length && !visited(ni)(nj) && board(ni)(nj) != '#') {
            stack = (ni, nj) :: stack
          }
        }
      }
    }

    // Check if any cell on the boundary has been visited
    val topRow = visited(0)
    val bottomRow = visited.last
    val leftColumn = visited.map(_(0))
    val rightColumn = visited.map(_.last)

    !topRow.contains(true) && !bottomRow.contains(true) && !leftColumn.contains(true) && !rightColumn.contains(true)
  }

  def getEnclosingWalls(board: Board): List[(Int, Int)] = {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

    //val playerPos = findPlayerPosition(board).getOrElse(return List())

    val visited = Array.fill(board.length, board(0).length)(false)
    val x = playerPos.x
    val y = playerPos.y
    var stack: List[(Int, Int)] = List((x, y))

    while (stack.nonEmpty) {
      val (i, j) = stack.head
      stack = stack.tail

      if (!visited(i)(j) && board(i)(j) != '#') {
        visited(i)(j) = true

        for ((di, dj) <- directions) {
          val (ni, nj) = (i + di, j + dj)
          if (ni >= 0 && ni < board.length && nj >= 0 && nj < board(ni).length && !visited(ni)(nj)) {
            stack = (ni, nj) :: stack
          }
        }
      }
    }

    // Identify walls adjacent to the visited area
    var walls = List[(Int, Int)]()
    for (i <- board.indices; j <- board(i).indices if visited(i)(j) && board(i)(j) != '#') {
      for ((di, dj) <- directions) {
        val (ni, nj) = (i + di, j + dj)
        if (ni >= 0 && ni < board.length && nj >= 0 && nj < board(ni).length && board(ni)(nj) == '#') {
          walls = (ni, nj) :: walls
        }
      }
    }

    walls.distinct
  }

  def isValidMap(board: Board): Boolean = {
    // Check if the map is a rectangle
    val colCount = board.headOption.map(_.length).getOrElse(0)
    if (board.exists(_.length != colCount)) return false

    // Check if there's only one player
    val playerCount = board.map(row => row.count(_ == 's')).sum
    if (playerCount != 1) return false

    // Check boundaries
    val topRow = board.head
    val bottomRow = board.last

    if (topRow.exists(_ != '#') || bottomRow.exists(_ != '#')) return false
    if (board.exists(row => row.head != '#' || row.last != '#')) return false

    // Check if walls around the player form an enclosed area
    if (!isWallsEnclosing(board)) return false

    true
  }
}
