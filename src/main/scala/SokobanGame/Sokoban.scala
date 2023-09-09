import scala.collection.mutable.Stack
package SokobanGame {
  sealed trait Tile
  case object Wall extends Tile
  case object Space extends Tile

  case class Point(x: Int, y: Int) {
    def +(that: Point) = Point(this.x + that.x, this.y + that.y)
  }

  object Sokoban extends App {
    type Board = Array[Array[Tile]];
    val mapModification = new MapModification();
    val game = new Game();

    var playerPos: Point = _
    var boxes: Set[Point] = Set.empty
    var targets: Set[Point] = Set.empty
    val moveStack: Stack[(Point, Set[Point])] = Stack.empty

    def loadLevel(level: String): Board = {
      val rows = level.split("\n")
      val board = Array.ofDim[Tile](rows.length, rows(0).length)

      for ((row, y) <- rows.zipWithIndex; (ch, x) <- row.zipWithIndex) {
        board(y)(x) = ch match {
          case '#' => Wall
          case '-' => Space
          case '.' => {
            targets += Point(x, y)
            Space
          }
          case 'x' => {
            boxes += Point(x, y)
            Space
          }
          case 's' => {
            playerPos = Point(x, y)
            Space
          }
          case '\r' => {
            Space
          }
        }
      }
      board
    }

    def printBoard(board: Board): Unit = {
      for (y <- board.indices; x <- board(y).indices) {
        if (Point(x, y) == playerPos) print('s')
        else if (boxes.contains(Point(x, y))) print('x')
        else board(y)(x) match {
          case Wall => print('#')
          case Space => print(if (targets.contains(Point(x, y))) '.' else '-')
          case _ => ()
        }
        if (x == board(y).length - 1) println()
      }
    }

    val moves = Map(
      'u' -> Point(0, -1),
      'd' -> Point(0, 1),
      'l' -> Point(-1, 0),
      'r' -> Point(1, 0)
    )

    val level =
      """-#####---
        |-#---####
        |-#---#--#
        |-##-----#
        |###-###-#
        |#sx.#-#-#
        |#---#-###
        |#---#----
        |#####----""".stripMargin

    var board = loadLevel(level)
    printBoard(board)

    game.Play(board);

  }
}



