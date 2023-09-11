package SokobanGame

import SokobanGame.Sokoban.{Board, playerPos}

import java.io.{File, PrintWriter}

class Solver {
  def findPlayerPosition1(grid: Array[Array[Tile]]): Point = {
    for (i <- grid.indices; j <- grid(i).indices) {
      if (grid(i)(j) == Player) return Point(i, j)
    }
    throw new Exception("Player not found!")
  }

  val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

  def findBoxes(grid: Array[Array[Tile]]): Set[Point] = {
    (for (i <- grid.indices; j <- grid(i).indices if grid(i)(j) == Box) yield Point(i, j)).toSet
  }

  def findTargets(grid: Array[Array[Tile]]): Set[Point] = {
    (for (i <- grid.indices; j <- grid(i).indices if grid(i)(j) == Target) yield Point(i, j)).toSet
  }

  def continue: Unit = {}
  def getMoves(grid: Board): List[String] = {

    val start = findPlayerPosition1(grid)
    val targets = findTargets(grid)
    val boxes = findBoxes(grid)

    def isValidMove(grid: Array[Array[Tile]], i: Int, j: Int): Boolean = {
      if (i < 0 || i >= grid.length || j < 0 || j >= grid(i).length) return false
      grid(i)(j) != Wall
    }

    @annotation.tailrec
    def bfs(queue: List[(Point, Set[Point], List[String])],
            visited: Set[(Point, Set[Point])]): List[String] = {
      if (queue.isEmpty) return List("No solution found")

      val (player, currentBoxes, path) = queue.head
      val restQueue = queue.tail

      if (currentBoxes.subsetOf(targets)) {
        return path
      }

      if (visited.contains((player, currentBoxes))) {
        bfs(restQueue, visited)
      } else {
        val nextSteps = for {
          (di, dj) <- directions
          ni = player.x + di
          nj = player.y + dj
          if isValidMove(grid, ni, nj) && !currentBoxes.contains(Point(ni, nj))
        } yield (Point(ni, nj), currentBoxes, path :+ mapDirections(di, dj))

        val pushes = for {
          (di, dj) <- directions
          ni = player.x + di
          nj = player.y+ dj
          if currentBoxes.contains(Point(ni, nj))
          nni = ni + di
          nnj = nj + dj
          if isValidMove(grid, nni, nnj) && !currentBoxes.contains(Point(nni, nnj))
        } yield (Point(ni, nj), currentBoxes - (Point(ni, nj)) + (Point(nni, nnj)), path :+ mapDirections(di, dj))

        bfs(restQueue ++ nextSteps ++ pushes, visited + ((player, currentBoxes)))
      }
    }

    bfs(List((start, boxes, List())), Set())
  }

  def solve(board: Board): Unit = {
    val moves = getMoves(board);
    val file = new File("C:\\games\\moves\\output.txt")
    val writer = new PrintWriter(file)
    try {
      moves.foreach(line => writer.println(line))
    }
    finally {
      writer.close()
    }
  }
  def mapDirections(direction: (Int, Int)) : String = {
    direction match {
      case (0,-1) => "l"
      case (0, 1) => "r"
      case (1,0) => "d"
      case (-1, 0) => "u"
    }
  }
}
